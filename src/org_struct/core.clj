(ns org-struct.core
  (:require [org-struct.matrix :refer [flatten-matrix matrix-columns matrix-ref
                                       matrix-rows paths]]))

(defn sum [xs]
  (list* '+ xs))

(defn matrix-inner-product [m1 m2]
  (sum (map #(list '* %1 %2)
            (flatten-matrix m1)
            (flatten-matrix m2))))

(defn org-struct [w t f q graph]
  (let [bp-count (matrix-rows t)
        contractors-count (matrix-columns t)
        P (paths graph)
        x (for [i (range bp-count)]
            (for [j (range contractors-count)]
              (gensym (str "x_" i "_" j "_"))))

        times (list* 'max (for [p P]
                            (sum (for [i p
                                       j (range contractors-count)]
                                   (list '*
                                         (matrix-ref x (- i 1) j)
                                         (matrix-ref t (- i 1) j))))))
        finances (matrix-inner-product x f)
        quality (sum (for [i (range bp-count)
                           j (range contractors-count)]
                       (list '*
                             (matrix-ref x i j)
                             (* (nth w i)
                                (matrix-ref q i j)))))]
    {:variables (into {} (map #(vector % :binary) (flatten x)))
     :objectives [[:minimize times]
                  [:minimize finances]
                  [:maximize quality]]
     :constraints (mapv #(list '= (sum %) 1) x)}))
