(ns org-struct.problem
  (:require [clojure.walk :refer [postwalk]]
            [numeric.expresso.core :refer [ex simplify]]
            [numeric.expresso.rules :refer [guard rule transform-one-level]]
            [org-struct.glpk :refer [glpk-solver]]
            [org-struct.schema :refer [Dir Function]]
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

(defn normalize [expr]
  (->> expr
      simplify
      wrap-in-product
      wrap-in-sum
      (postwalk #(transform-one-level normalize-rules %))))

(defn normalize-constraint [constraint]
  (let [[op func num] constraint]
    [op (normalize func) num]))

(def ^:dynamic *lp-solver* glpk-solver)

(s/defn solve-lp [variables dir :- Dir f constraints]
  (*lp-solver* variables dir (normalize f) (map normalize-constraint constraints)))

(defn solve-result [& args]
  (:result (apply solve-lp args)))

(defn solve-germeyer [weights {:keys [variables objectives constraints]}]
  (let [minmaxes (for [[dir func] objectives]
                   (do
                     (s/validate Dir dir) ; TODO
                     [(solve-result :minimize func constraints)
                      (solve-result :maximize func constraints)]))
        funcs (for [[[fmin fmax] [dir func]] (interleave minmaxes objectives)]
                (case dir
                  :minimize #(ex (/ (- func fmin) (- fmax fmin)))
                  :maximize #(ex (/ (- fmax func) (- fmax fmin)))))
        weighted-funcs (map #(ex (* ~%1 ~%2)) funcs weights)]
    (solve-lp variables :minimize (ex (max ~@weighted-funcs)) constraints)))
