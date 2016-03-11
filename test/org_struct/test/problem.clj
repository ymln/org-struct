(ns org-struct.test.problem
  (:require [clojure.test :refer [are deftest is]]
            [org-struct.problem :refer [normalize solve-lp]]))

(deftest normalization
  (are [a b] (= (normalize a) b)
       '(+ x y) '(+ (* 1 y) (* 1 x))
       '(+ (* 5 x) (* y 6) z) '(+ (* 1 z) (* 5 x) (* 6 y))
       '(+ (* x 4)) '(+ (* 4 x))
       '(+ (* 5 (- x))) '(+ (* -5 x))
       'x '(+ (* 1 x))))

(deftest lp-test
  (is (= '{x 2. y 3.} (solve-lp {} :maximize 'x '[(<= (+ x y) 5) (>= y 3)]))))
