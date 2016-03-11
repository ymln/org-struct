(ns org-struct.glpk
  (:require [org-struct.schema :refer [Dir Solution Constraint]]
            [org-struct.utils :refer [indexed find-symbols func-vals]]
            [schema.core :as s])
  (:import (clojure.asm Type)
           (java.util.function Function)
           (org.gnu.glpk GLPK GLPKConstants glp_smcp)))

(defn glpk-populate-array! [arr type vals]
  (doseq [[val i] (indexed vals)]
    (case type
      :int    (GLPK/intArray_setitem    arr (inc i) val)
      :double (GLPK/doubleArray_setitem arr (inc i) val))))

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

