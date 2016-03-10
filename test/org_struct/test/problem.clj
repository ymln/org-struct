(ns org-struct.test.problem
  (:require [clojure.test :refer [are deftest]]
            [numeric.expresso.core :refer [ex]]
            [org-struct.problem :refer [normalize]]))

(deftest normalization
  (are [a b] (= (normalize a) b)
       '(+ x y) '(+ (* 1 y) (* 1 x))
       '(+ (* 5 x) (* y 6) z) '(+ (* 1 z) (* 5 x) (* 6 y))
       '(+ (* x 4)) '(+ (* 4 x))
       '(+ (* 5 (- x))) '(+ (* -5 x))
       'x '(+ (* 1 x))))
