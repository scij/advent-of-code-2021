(ns advent-of-code-2021.day05
  (:require [advent-of-code-2021.util :as util]))

(defn load-file [fn]
  (with-open [rdr (clojure.java.io/reader fn)]
    (->> (doall (line-seq rdr))
         (map (partial re-seq #"(\d+),(\d+) -> (\d+),(\d+)"))
         (map first)                                        ; unwrap enclosing list
         (map next)                                         ; unwrap re-seq all-match
         (map #(map util/to-int %))
         )
    )
  )

(defn range-up [i1 i2]
  (if (< i1 i2)
    (range i1 (inc i2))
    (range i1 (dec i2) -1)))

(defn is-regular-line [[x1 x2 y1 y2]]
  (or
    (= x1 x2)
    (= y1 y2)
    (= (Math/abs ^int (- x1 x2)) (Math/abs ^int (- y1 y2)))
    )
  )

(defn line-points [x1 y1 x2 y2]
  (println "line points" x1 y1 x2 y2)
  (cond
    (= x1 x2)
    (for [x [x1]
          y (range-up y1 y2)]
      [x y]
      )
    (= y1 y2)
    (for [x (range-up x1 x2)
          y [y1]]
      [x y]
      )
    (= (Math/abs ^int (- y1 y2)) (Math/abs ^int (- x1 x2)))
    (partition 2 (interleave (range-up x1 x2) (range-up y1 y2)))
    )
  )


(defn set-point [matrix [x y]]
  (if (contains? matrix [x y])
    (assoc matrix [x y] (inc (get matrix [x y])))
    (assoc matrix [x y] 1))
  )

(defn draw-line [matrix points]
  (println "draw line" points)
  (reduce #(set-point %1 %2) matrix points)
  )

(defn draw-lines [matrix start-end-points]
  (println "draw lines" (first start-end-points))
  (if (empty? start-end-points)
    matrix
    (let [[x1 y1 x2 y2] (first start-end-points)
          points (line-points x1 y1 x2 y2)]
      (draw-lines (draw-line matrix points) (rest start-end-points))
      )
    )
  )

(defn dump-matrix [matrix]
  (doseq [y (range 0 10)]
    (doseq [x (range 0 10)]
      (if (contains? matrix [x y])
        (print (get matrix [x y]))
        (print "."))
      )
    (println)
    )
  )