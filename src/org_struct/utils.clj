(ns org-struct.utils
  (:require [org-struct.schema :refer [Function]]
            [schema.core :as s]))

(defn indexed [coll]
  (map-indexed (fn [x i] [i x]) coll))

(defn find-symbols [xs]
  (set (filter symbol? (tree-seq sequential?
                                 #(if (symbol? (first %))
                                    (rest %)
                                    %)
                                 xs))))

(s/defn func-vals :- [s/Num]
  [func :- Function, vars :- [s/Symbol]]
  (prn "FUNC: " func)
  (let [vars-map (into {} (map (fn [[op num var]]
                                 (assert (= op '*))
                                 [var num])
                               (rest func)))]
    (map #(get vars-map % 0) vars)))
