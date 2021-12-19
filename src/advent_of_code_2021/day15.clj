(ns advent-of-code-2021.day15
  (:require [clojure.string :as s]
            [loom.graph :as g]
            [loom.alg :as alg]
            [advent-of-code-2021.util :refer :all]))


(defn load-file [fn]
  (with-open [rdr (clojure.java.io/reader fn)]
    (->> (doall (line-seq rdr))
         (map #(s/split % #""))
         (map #(map to-int %))
         (map vec)
         vec
         )
    )
  )

(defn neighbor-pos
  "returns a vector of neighbors for the given position"
  [matrix x y max-x max-y]
  (->>
    [[x (inc y)]                                            ; down
     [(inc x) y]                                            ; right
     [(dec x) y]                                            ; left
     [x (dec y)]                                            ; up
     ]
    (filter (fn [[x y]] (and (>= x 0) (>= y 0) (<= x max-x) (<= y max-y))))
    (sort (fn [a b] (compare (get-in matrix [(second a) (first a)])
                             (get-in matrix [(second b) (first b)])))
          )
    )
  )

(defn- node-weight [matrix [x y]]
  (get-in matrix [y x])
  )

(defn load-matrix-into-graph [matrix]
  (let [max-x (dec (count (first matrix)))
        max-y (dec (count matrix))]
    (g/weighted-digraph
      (into {}
            (for [x (range (inc max-x))
                  y (range (inc max-y))]
              [[x y] (apply merge
                            (map
                              #(hash-map % (node-weight matrix %))
                              (neighbor-pos matrix x y max-x max-y)))])))
    )
  )

(defn part1 [fn]
  (let [matrix (load-file fn)
        graph (load-matrix-into-graph matrix)]
    (apply +
           (map
             #(node-weight matrix %)
             (alg/dijkstra-path graph [0 0] [(dec (count (first matrix))) (dec (count matrix))])
             ))
    )
  )

(defn inc-risk [d by]
  (let [n (+ d by)]
    (if (> n 9)
      (- n 9) n)))

(defn scale-line-right [line]
  (vec
    (apply concat
           (for [i (range 5)]
             (mapv #(inc-risk % i) line)))))

(defn scale-matrix-right [matrix]
  (mapv
    scale-line-right
    matrix))

(defn scale-line-downward [line by]
  (mapv
    #(inc-risk % by)
    line))

(defn scale-matrix-downward [matrix]
  (vec (apply concat
          (for [i (range 5)]
            (mapv #(scale-line-downward % i) matrix)))))

(defn part2 [fn]
  (let [matrix (load-file fn)
        matrix-s-r (scale-matrix-right matrix)
        matrix-s-d (scale-matrix-downward matrix-s-r)
        graph (load-matrix-into-graph matrix-s-d)]
    (apply +
           (map
             #(node-weight matrix-s-d %)
             (alg/dijkstra-path graph
                                [0 0]
                                [(dec (count (first matrix-s-d))) (dec (count matrix-s-d))])))
    )
  )