(ns org-struct.test.problem
  (:require [clojure.test :refer [are deftest is]]
            [org-struct.problem :refer [normalize normalize* remove-extremums
                                        solve-germeyer solve-lp-result
                                        vectorize]]
            [org-struct.utils :refer [has-map?]]))

(deftest normalization
  (are [a b] (= (normalize a) b)
       '(+ x y) '(+ (* 1 x) (* 1 y))
       '(+ (* 5 x) (* y 6) z) '(+ (* 5 x) (* 6 y) (* 1 z))
       '(+ (* x 4)) '(+ (* 4 x))
       '(+ (* 5 (- x))) '(+ (* -5 x))
       'x '(+ (* 1 x)))
       '(+ (* 1 x) (* 1 y) [- z]) '(+ (* 1 x) (* 1 y) (* -1 z)))

(deftest lp-test
  (is (= '{x 2. y 3.} (solve-lp-result {} :maximize 'x '[(<= (+ x y) 5) (>= y 3)]))))

(defn seq-eq? [s tpl]
  (if (and (sequential? s)
           (sequential? tpl))
    (every? (fn [[x y]] (seq-eq? x y)) (map vector s tpl))
    (or (= tpl '_) (= s tpl))))

(deftest remove-extremums-test
  (are [a b] (seq-eq? (remove-extremums a) b)
       '(max x y) '[_ [(<= (- x _)) (<= (- y _))]]
       '(+ (max x y) (+ (min y (max a b)) 3))
         '[(+ _ (+ _ 3)) [(<= (- x _) 0) (<= (- y _) 0)
                          (>= (- y _) 0) (>= (- (max a b) _) 0)]]))

(def constraints '[(<= (+ x y z) 9)
                   (<= x 9)
                   (<= y 9)
                   (<= z 9)
                   (>= x 0)
                   (>= y 0)
                   (>= z 0)])
(deftest germeyer-test
  (is (has-map? '{x 3. y 3. z 3.}
                (:result (solve-germeyer [1 1 1]
                                         {:variables {}
                                          :objectives '[[:maximize x]
                                                        [:maximize y]
                                                        [:maximize z]]
                                          :constraints constraints})))))

(deftest germeyer-test2
  (is (has-map? '{x 10. y 0.} (:result (solve-germeyer [1 1]
                                                       {:variables {}
                                                        :objectives '[[:maximize x]
                                                                      [:minimize y]]
                                                        :constraints '[(<= (max x y) 10)
                                                                       (>= y 0)
                                                                       (>= x 0)]})))))

(deftest germeyer-test3
  (is (has-map? '{x 0.75 y 1.5 z 2.25} (:result (solve-germeyer [1 1]
                                                                {:variables {}
                                                                 :objectives '[[:minimize (max (* 2 x) y)]
                                                                               [:maximize z]]
                                                                 :constraints '[(<= (max x y z) 3)
                                                                                (>= y 0)
                                                                                (>= x 0)
                                                                                (>= z 0)
                                                                                (= (+ x y (- z)) 0)]})))))

(deftest binary-test
  (is (has-map? '{x 1 y 0 z 1}
                (solve-lp-result '{x :binary, y :binary, z :binary}
                                 :maximize
                                 '(+ x z)
                                 '[(<= (+ x y) 1)
                                   (= (max y z) 1)]))))

(deftest other-tests
  (is (= '[+ 1 [+ 2 3]] (vectorize '(+ 1 (+ 2 3)))))
  (is (= 6 (normalize* '(+ 1 (+ 2 3)))))
  (is (= 0 (normalize* '(* 0 x y z))))
  (is (= '(+ 3 x) (normalize* '(+ 1 x 2))))
  (is (= '(* -5 x) (normalize* '(* 5 (- x)))))
  (is (= '(+ 15 (* 5 x)) (normalize* '(* 5 (+ 3 x)))))
  (is (= (normalize 'x) '(+ (* 1 x)))))
