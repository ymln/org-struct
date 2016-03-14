(ns org-struct.problem
  (:require [clojure.walk :refer [postwalk]]
            [numeric.expresso.core :refer [ex optimize simplify]]
            [numeric.expresso.optimize :refer [compile-expr*]]
            [numeric.expresso.rules :refer [guard rule transform-one-level]]
            [org-struct.glpk :refer [glpk-solver]]
            [org-struct.schema :refer [Dir]]
            [org-struct.utils :refer [find-symbols]]
            [schema.core :as s]))

(def normalize-rules
  [(rule (ex (+ ?x ?&*)) :=> (ex (+ (* 1 ?x) ?&*)) :if (guard (symbol? ?x)))
   (rule (ex (* ?x)) :=> (ex (* 1 ?x)) :if (guard (symbol? ?x)))
   (rule (ex (* ?num (- ?x))) :==> (list '* (- ?num) ?x) :if (guard (number? ?num)))
   (rule (ex (* ?x ?num)) :=> (ex (* ?num ?x)) :if (guard (number? ?num)))])

(defn wrap-in-product [expr]
  (if-not (seq? expr)
    (list '* expr)
    expr))

(defn wrap-in-sum [expr]
  (if (= '* (first expr))
    (cons '+ (list expr))
    expr))

(defn fix-minus [expr]
  (cons (first expr)
        (for [x (rest expr)]
          (if (and (sequential? x)
                   (= 2 (count x))
                   (= '- (first x))
                   (symbol? (second x)))
            (list '* -1 (second x))
            x))))

(defn normalize [expr]
  (->> expr
       simplify
       wrap-in-product
       wrap-in-sum
       fix-minus
       (postwalk #(transform-one-level normalize-rules %))))

(defn simplify-constraint [constraint]
  (let [[op func num] constraint]
    (if (and (sequential? func)
             (= '+ (first func)))
      (let [numbers (filter number? func)
            not-numbers (filter #(not (number? %)) func)]
        (list op not-numbers (- num (apply + numbers))))
      constraint)))

(defn normalize-constraint [constraint]
  (let [[op func num] constraint]
    (simplify-constraint (list op (normalize func) num))))

(def ^:dynamic *lp-solver* glpk-solver)

(defn remove-extremums [f]
  (if (sequential? f)
    (let [[op & args] f]
      (if (or (= op 'min)
              (= op 'max))
        (let [new-var (gensym (str op))
              sign (case op
                     min '>=
                     max '<=)
              constraints (map #(list sign (list '- % new-var) 0) args)]
          [new-var constraints])
        (let [rex (map remove-extremums args)
              new-f (cons op (map first rex))
              constraints (vec (mapcat second rex))]
          [new-f constraints])))
    [f []]))

(defn remove-extremums-in-constraints [constraints]
  (loop [processed [], unprocessed constraints]
    (if-not (empty? unprocessed)
      (let [constraint (first unprocessed)
            [op func num] constraint
            [new-func new-constraints] (remove-extremums func)]
        (recur (conj processed (list op new-func num)) (concat (rest unprocessed) new-constraints)))
      processed)))

(defn evaluate-ex [func args]
  (let [vars (vec (find-symbols func))
        f (compile-expr* vars (optimize func))]
    (apply f (mapv #(get args % 0) vars))))

(def solve-lp-result)
(s/defn solve-lp [variables dir :- Dir f constraints]
  (let [should-recurse? (and (sequential? f)
                             (or (and (= 'max (first f))
                                      (= :maximize dir))
                                 (and (= 'min (first f))
                                      (= :minimize dir))))
        [new-f constraints-for-f] (if should-recurse?
                                    [f []]
                                    (remove-extremums f))
        new-constraints (remove-extremums-in-constraints (concat constraints constraints-for-f))]
    (if should-recurse?
      {:result (last (sort-by #(evaluate-ex new-f %) (map #(solve-lp-result variables dir % constraints) (rest new-f))))}
      (*lp-solver* variables dir (normalize new-f) (map normalize-constraint new-constraints)))))

(defn solve-lp-result [& args]
  (let [solution (apply solve-lp args)]
    (or (:result solution)
        (throw (Exception. (str "Error " (:error solution) " when trying to solve problem"))))))

(defn solve-germeyer [weights {:keys [variables objectives constraints]}]
  (assert (= (count weights) (count objectives)))
  (let [min+max (for [[dir func] objectives]
                   [(evaluate-ex func (solve-lp-result variables :minimize func constraints))
                    (evaluate-ex func (solve-lp-result variables :maximize func constraints))])
        funcs (for [[[fmin fmax] [dir func]] (map vector min+max objectives)]
                (if (= fmax fmin)
                  (throw (Exception. (str "Error when trying to solve problem: "
                                          fmax " is equal to " fmin
                                          " for function " func)))
                  (case dir
                    :minimize (ex (/ (- ~func ~fmin) (- ~fmax ~fmin)))
                    :maximize (ex (/ (- ~fmax ~func) (- ~fmax ~fmin))))))
        weighted-funcs (map #(ex (* ~%1 ~%2)) funcs weights)]
    (solve-lp-result variables
                     :minimize
                     (list* 'max weighted-funcs)
                     constraints)))
