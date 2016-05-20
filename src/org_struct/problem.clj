(ns org-struct.problem
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :refer [postwalk]]
            [org-struct.glpk :refer [glpk-solver]]
            [org-struct.schema :refer [Dir]]
            [org-struct.utils :refer [find-symbols p]]
            [schema.core :as s]))

(comment (def normalize-rules
           [(rule (ex (+ ?x ?&*)) :=> (ex (+ (* 1 ?x) ?&*)) :if (guard (symbol? ?x)))
            (rule (ex (* ?x)) :=> (ex (* 1 ?x)) :if (guard (symbol? ?x)))
            (rule (ex (* ?num (- ?x))) :==> (list '* (- ?num) ?x) :if (guard (number? ?num)))
            (rule (ex (* ?x ?num)) :=> (ex (* ?num ?x)) :if (guard (number? ?num)))]))

(defn wrap-in-product [expr]
  (if-not (sequential? expr)
    (if-not (number? expr)
      (list '* 1 expr)
      expr)
    (if (= '+ (first expr))
      (list* '+ (map wrap-in-product (rest expr)))
      expr)))

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

(defn third [xs]
  (second (rest xs)))

; (rule (ex (* ?num (+ ?x ?&*))) :=> (ex (+ (* ?num ?x) (* ?num (+ ?&*)))))
(defn fix-product [expr]
  (if (and (sequential? expr)
           (= 3 (count expr))
           (= '* (first expr))
           (sequential? (third expr))
           (= '+ (first (third expr))))
    (let [num (second expr)
          addends (rest (third expr))]
      (list* '+ (map #(list '* num %) addends)))
    expr))

; (rule (ex (+ (+ &x*) &y*)) :=> (ex (+ &x* &y*)))
(defn fix-sum+prod [expr]
  (if (and (sequential? expr)
           ('#{+ *} (first expr)))
    (let [op (first expr)
          res (reduce (fn [res subexpr]
                        (if (and (sequential? subexpr)
                                 (= op (first subexpr)))
                          (update-in res [:plus] concat (rest subexpr))
                          (update-in res [:non-plus] conj subexpr)))
                      {:plus [] :non-plus []}
                      (rest expr))]
      (list* op (concat (:plus res) (:non-plus res))))
    expr))

(defn numbers? [xs]
  (every? number? xs))

(defn not-1 [x]
  (not= 1 x))

(defn not-number? [x]
  (not (number? x)))

(defn normalize-step [expr]
  (match expr
    [(op :guard '#{+ - * /}) & (xs :guard numbers?)]
      (apply (case op + + - - * * / /) xs)
    ['- x] ['* -1 x]
    ['- x & xs] (vector '+ x (apply vector '* -1 xs))
    ['/ (x :guard not-1) & (xs :guard not-empty)] (vector '* x (apply vector '/ 1 xs))
    ['* 0 & xs] 0
    ['* 1 & xs] (apply vector '* xs)
    ['+ 0 & xs] (apply vector '+ xs)
    ['+ x] x
    ['* x] x
    ['* x ['+ & xs]] (apply vector '+ (map #(vector '* x %) xs))
    ['* (x :guard number?) ['- (y :guard not-number?)]] (vector '* (- x) y)
    [(op :guard '#{* +}) & xs]
      (let [numbers (filter number? xs)
            not-numbers (filter not-number? xs)]
        (if (empty? numbers)
          (apply vector op not-numbers)
          (apply vector op (apply (case op + + * *) numbers) not-numbers)))
    [(op :guard '#{min max}) & (xs :guard numbers?)]
      (apply (case op min min max max) xs)
    :else expr))

(defn vectorize [expr]
  (postwalk #(if (seq? %) (vec %) %) expr))

(defn normalize* [expr]
  (loop [n 0 expr (vectorize expr)]
    (if (> n 1000)
      (throw (Exception. "Normalization limit"))
      (let [new-expr (postwalk #(vectorize (fix-sum+prod (normalize-step %))) expr)]
        (if (not= expr new-expr)
          (recur (inc n) new-expr)
          new-expr)))))

(defn normalize [expr]
  (->> expr
       normalize*
       wrap-in-product
       wrap-in-sum))

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

(comment (defn evaluate-ex [func args]
           (let [vars (vec (find-symbols func))
                 f (compile-expr* vars (optimize func))]
             (apply f (mapv #(get args % 0) vars)))))

(defn evaluate-ex [func args]
  (let [res (normalize* (postwalk #(or (args %) %) func))]
    (if (sequential? res)
      (case (first res)
        min (apply min (rest res))
        max (apply max (rest res)))
      res)))

(declare solve-lp-result)
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
        _ (prn min+max)
        funcs (for [[[fmin fmax] [dir func]] (map vector min+max objectives)]
                (if (= fmax fmin)
                  (throw (Exception. (str "Error when trying to solve problem: "
                                          fmax " is equal to " fmin
                                          " for function " func)))
                  (case dir
                    :minimize ['/ ['- func fmin] ['- fmax fmin]]
                    :maximize ['/ ['- fmax func] ['- fmax fmin]])))
        weighted-funcs (map #(vector '* %1 %2) funcs weights)
        obj-func (list* 'max weighted-funcs)]
    (assert (numbers? (apply concat min+max)))
    {:obj-func obj-func
     :result (solve-lp-result variables
                              :minimize
                              obj-func
                              constraints)}))
