(ns org-struct.matrix)

(defn graph-vertices [graph]
  (into #{} (apply concat graph)))

(defn graph-next [graph v]
  (map second (filter (fn [[v1 v2]] (= v1 v)) graph)))

(defn graph-last? [graph v]
  (empty? (graph-next graph v)))

(defn graph-seconds [graph]
  (map second graph))

(defn graph-is-start? [graph vertice]
  (not (some #{vertice} (graph-seconds graph))))

(defn graph-starts [graph]
  (filter (partial graph-is-start? graph) (graph-vertices graph)))

(defmacro defmemofn [name & rest]
  `(def ~name (memoize (fn ~@rest))))

(defmemofn paths-from [graph start]
  (if (graph-last? graph start)
    [[start]]
    (let [after-start (graph-next graph start)
          paths-from-after-start (mapcat (partial paths-from graph) after-start)]
      (map (partial cons start) paths-from-after-start))))

(defn paths [graph]
  (let [starts (graph-starts graph)]
    (mapcat (partial paths-from graph) starts)))

(defn matrix-rows [m]
  (count m))

(defn matrix-columns [m]
  (count (first m)))

(defn matrix-column [m col]
  (mapv #(nth % col) m))

(defn matrix-row [m row]
  (nth m row))

(defn matrix-ref [m row col]
  (-> (matrix-row m row)
      (nth col)))

(defn flatten-matrix [m]
  (apply concat m))
