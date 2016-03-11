(ns org-struct.problem
  (:require [clojure.walk :refer [postwalk]]
            [numeric.expresso.core :refer [ex simplify]]
            [numeric.expresso.rules :refer [guard rule transform-one-level]]
            [schema.core :as s])
  (:import (org.gnu.glpk GLPK GLPKConstants glp_smcp)))

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

(def Dir (s/enum :minimize :maximize))
(def Product [(s/one (s/eq '*) "*") (s/one s/Num "Num") (s/one s/Symbol "Symbol")])
(def Function [(s/one (s/eq '+) "+") Product])
(def Condition (s/enum '<= '= '>=))
(def Constraint [(s/one Condition "Condition")
                 (s/one Function "Function")
                 (s/one s/Num "Num")])

(def Type (s/enum :binary))
(def Solution s/Any) ; TODO

(defn find-symbols [xs]
  (set (filter symbol? (tree-seq sequential? rest xs))))

(defn indexed [coll]
  (map-indexed (fn [x i] [i x]) coll))

(defn glpk-populate-array! [arr type vals]
  (doseq [[val i] (indexed vals)]
    (case type
      :int    (GLPK/intArray_setitem    arr (inc i) val)
      :double (GLPK/doubleArray_setitem arr (inc i) val))))

(s/defn func-vals :- [s/Num]
  [func :- Function, vars :- [s/Symbol]]
  (let [vars-map (into {} (map (fn [[op num var]]
                                 (assert (= op '*))
                                 [var num])
                               (rest func)))]
    (map #(get vars-map % 0) vars)))

(s/defn glpk-solver :- Solution
  [variables :- {s/Symbol Type}
   dir :- Dir
   f :- Function
   constraints :- [Constraint]]
  (let [problem (GLPK/glp_create_prob)
        vars (vec (find-symbols (conj constraints f)))
        vars-count (count vars)
        int-array (GLPK/new_intArray (inc vars-count))
        double-array (GLPK/new_doubleArray (inc vars-count))
        params (glp_smcp.)]
    (try
      (GLPK/glp_add_cols problem vars-count)
      (doseq [[var i] (indexed vars)]
        (GLPK/glp_set_col_name problem (inc i) (str var))
        (GLPK/glp_set_col_bnds problem (inc i) GLPKConstants/GLP_FR 0 0)
        (GLPK/glp_set_col_kind problem (inc i) (case (variables var)
                                                 :binary GLPKConstants/GLP_BV
                                                 GLPKConstants/GLP_CV)))

      (glpk-populate-array! int-array :int (range 1 (inc vars-count)))
      (GLPK/glp_add_rows problem (count constraints))
      (doseq [[[condition func num] i] (indexed constraints)]
        (GLPK/glp_set_row_bnds problem (inc i)
                               (case condition
                                 <= GLPKConstants/GLP_UP
                                 >= GLPKConstants/GLP_LO
                                 =  GLPKConstants/GLP_FX)
                               num num)
        (glpk-populate-array! double-array :double (func-vals func vars))
        (GLPK/glp_set_mat_row problem (inc i) vars-count int-array double-array))

      (GLPK/glp_set_obj_dir problem (case dir
                                      :minimize GLPKConstants/GLP_MIN
                                      :maximize GLPKConstants/GLP_MAX))
      (doseq [[x i] (indexed (func-vals f vars))]
        (GLPK/glp_set_obj_coef problem (inc i) x))
      (GLPK/glp_init_smcp params)

      (let [result (GLPK/glp_simplex problem params)]
        (if (= 0 result)
          (into {} (map (fn [[var i]] [var (GLPK/glp_get_col_prim problem (inc i))])
                        (indexed vars))) ; solution
          {:error result}))
      (finally
        ;(GLPK/glp_write_lp problem nil "/tmp/lp.lp")
        (GLPK/delete_intArray int-array)
        (GLPK/delete_doubleArray double-array)
        (GLPK/glp_delete_prob problem)))))

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
