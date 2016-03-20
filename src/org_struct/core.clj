(ns org-struct.core
  (:require [org-struct.matrix :refer [flatten-matrix matrix-columns matrix-ref
                                       matrix-rows paths]]
            [org-struct.problem :refer [evaluate-ex solve-germeyer]]))

(def oo Double/POSITIVE_INFINITY)

(defn sum [xs]
  (list* '+ xs))

(defn matrix-inner-product [m1 m2]
  (sum (map #(list '* %1 %2)
            (flatten-matrix m1)
            (flatten-matrix m2))))

(defn same-dimensions? [m1 m2]
  (and (= (count m1) (count m2))
       (every? (fn [[x1 x2]] (= (count x1) (count x2))) (map vector m1 m2))))

(defn org-struct [weights w t f q graph]
  (assert (same-dimensions? t f))
  (assert (same-dimensions? t q))
  (let [bp-count (matrix-rows t)
        contractors-count (matrix-columns t)
        P (paths graph)
        x (for [i (range bp-count)]
            (for [j (range contractors-count)]
              (gensym (str "x_" i "_" j "_"))))

        times (list* 'max (for [p P]
                            (sum (for [i p
                                       j (range contractors-count)]
                                   (if (number? i)
                                     (let [t* (matrix-ref t (- i 1) j)]
                                       (if (not= t* oo)
                                         (list '* (matrix-ref x (- i 1) j) t*)
                                         0))
                                     0)))))
        finances (sum (for [i (range bp-count)
                            j (range contractors-count)]
                        (let [f* (matrix-ref f i j)]
                          (if (not= f* oo)
                            (list '* (matrix-ref x i j) f*)
                            0))))
        quality (sum (for [i (range bp-count)
                           j (range contractors-count)]
                       (let [q* (matrix-ref q i j)]
                         (if (not= q* 0)
                           (list '*
                                 (matrix-ref x i j)
                                 (* (nth w i) q*))
                           0))))
        invalid-xs (for [i (range bp-count)]
                     (filter #(not (nil? %))
                             (for [j (range contractors-count)]
                               (if (= oo (matrix-ref t i j))
                                 (matrix-ref x i j)))))
        invalid-xs-constraints (map #(vector '= % 0) (flatten invalid-xs))
        res (solve-germeyer
              weights
              {:variables (into {} (map #(vector % :binary) (flatten x)))
               :objectives [[:minimize times]
                            [:minimize finances]
                            [:maximize quality]]
               :constraints (concat (mapv #(list '= (sum %) 1) x)
                                    invalid-xs-constraints)})]
    {:quality  (evaluate-ex quality  res)
     :finances (evaluate-ex finances res)
     :time     (evaluate-ex times    res)}))
