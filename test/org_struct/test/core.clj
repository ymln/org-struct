(ns org-struct.test.core
  (:require [clojure.test :refer [is]]
            [org-struct.core :refer [oo org-struct]]
            [org-struct.utils :refer [has-map?]]
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
  (let [res (org-struct [1/3 1/3 1/3] w t f q graph)]
    (prn (:obj-result res))
    (is (or (has-map? {:quality 135 :finances 50 :time 15} res)
            (has-map? {:quality 751 :finances 70 :time 10} res)
            (has-map? {:quality 765 :finances 85 :time 21} res))
        (str "Result is " res))))

(deftest org-struct-test-simple
  (is (has-map? {:quality 4 :finances 2 :time 2}
                (org-struct [1 1 1] [1 1] [[1 2] [1 2]] [[1 2] [1 2]] [[2 1] [2 1]] [[1 2]]))))
