(ns org-struct.test.core
  (:require [clojure.test :refer [is]]
            [org-struct.core :refer [oo org-struct]]
            [schema.test :refer [deftest]]))

(def t
  [[5  3   oo]
   [4  5   oo]
   [4  3   oo]
   [15 10  8  ]
   [15 6   oo]
   [20 oo  3  ]
   [25 oo  4  ]
   [30 15  oo]])

(def f
  [[5  10  oo]
   [5  5   oo]
   [5  5   oo]
   [10 5   5]
   [25 15  oo]
   [30 oo  10]
   [40 oo  30]
   [50 15  oo]])

(def q
  [[10 3 0]
   [10 4 0]
   [10 4 0]
   [10 6 2]
   [9  9 0]
   [5  0 8]
   [6  0 7]
   [8  7 0]])

(def w [30 20 10 20 10 5 3 2])

(def graph
  '[[6 b]
    [5 b]
    [2 b]
    [b 7]
    [b 8]
    [b 3]
    [7 c]
    [8 c]
    [3 c]
    [c 4]
    [4 d]
    [d e]
    [a 1]
    [1 e]])

(deftest org-struct-test
  (is (= {:time 15 :finances 50 :quality 135}
         (org-struct [1/3 1/3 1/3] w t f q graph))))
