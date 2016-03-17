(ns org-struct.test.core
  (:require [org-struct.core :refer [org-struct]]
            [org-struct.problem :refer [solve-germeyer]]
            [schema.test :refer [deftest]]))

(def oo 100000000) ;; infinity

(def t
  [[5  3   oo  oo  oo]
   [4  5   oo  oo  oo]
   [4  3   oo  oo  oo]
   [10 8   3   5   oo]
   [15 6   oo  5   oo]
   [20 oo  3   5   oo]
   [20 oo  4   7   oo]
   [30 15  oo  15  20]])

(def f
  [[5000  10000 oo    oo    oo]
   [5000  5000  oo    oo    oo]
   [5000  5000  oo    oo    oo]
   [20000 15000 5000  10000 oo]
   [25000 15000 oo    15000 oo]
   [30000 oo    10000 15000 oo]
   [40000 oo    30000 25000 oo]
   [50000 15000 oo    15000 25000]])

(def q
  [[10 3 0 0 0]
   [10 4 0 0 0]
   [10 4 0 0 0]
   [10 6 2 5 0]
   [9  9 0 7 0]
   [5  0 8 9 0]
   [6  0 7 8 0]
   [8  7 0 7 9]])

(def w [30 20 10 20 10 5 3 2])

(def graph
  [[6 2]
   [5 2]
   [2 7]
   [2 8]
   [7 3]
   [8 3]
   [3 4]
   [4 1]])

(deftest org-struct-test
  (prn "SOLUTION: " (solve-germeyer [1 1 1] (org-struct w t f q graph))))
