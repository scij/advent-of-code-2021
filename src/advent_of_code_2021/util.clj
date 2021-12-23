(ns advent-of-code-2021.util)

(defn to-int [s]
  (Integer/parseInt s))

(defn permutations [coll]
  (if (= 1 (count coll))
    (list coll)
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn println-ret [v]
  (println v)
  v)

