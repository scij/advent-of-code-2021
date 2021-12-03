(ns advent-of-code-2021.day03
  (:require [clojure.string :as s]))

(defn- str-to-int [s]
  (Integer/parseInt s))

(defn- bitvec-to-int [v]
  (reduce (fn [acc bit]
            (bit-or (bit-shift-left acc 1) bit))
          v)
  )

(defn read-file
  "returns the content of fn as a list of lists of binary digits"
  [fn]
  (map
    #(map str-to-int (re-seq #"\d" %))
    (s/split
      (slurp fn) #"\n"))
  )

(defn gamma [matrix]
  (let [t-matrix (apply mapv vector matrix)
        rows (count matrix)
        r-sum (map (partial reduce +) t-matrix)
        most-frequent (map #(if (> % (/ rows 2)) 1 0) r-sum)]
    (bitvec-to-int most-frequent)
    )
  )

(defn epsilon [gamma matrix]
  (dec (- (int (Math/pow 2 (count (first matrix)))) gamma))
  )

(defn most-common-val [v]
  (if (>= (reduce + v) (/ (count v) 2)) 1 0))

(defn least-common-val [v]
  (if (< (reduce + v) (/ (count v) 2)) 1 0))

(defn has-val-in-col? [val col row]
  (= val (nth row col))
  )

(defn oxygen-generator-rating [col matrix]
  (let [t-matrix (apply mapv vector matrix)
        mcv (most-common-val (nth t-matrix col))
        remaining-matrix (filter (partial has-val-in-col? mcv col) matrix)]
    (if (= 1 (count remaining-matrix))
      (bitvec-to-int (first remaining-matrix))
      (oxygen-generator-rating (inc col) remaining-matrix))
    )
  )

(defn co2-scrubber-rating [col matrix]
  (let [t-matrix (apply mapv vector matrix)
        lcv (least-common-val (nth t-matrix col))
        remaining-matrix (filter (partial has-val-in-col? lcv col) matrix)]
    (if (= 1 (count remaining-matrix))
      (bitvec-to-int (first remaining-matrix))
      (co2-scrubber-rating (inc col) remaining-matrix))
    )
  )
