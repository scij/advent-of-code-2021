(ns advent-of-code-2021.day02
  (:require [clojure.string :as s]))


(defn update-pos [[x y] cmd]
  (let [[dir val-s] (s/split cmd #" ")
        val (Integer/parseInt val-s)]
    (cond
      (= "forward" dir) {:x (+ x val) :y y}
      (= "up" dir) {:x x :y (- y val)}
      (= "down" dir) {:x x :y (+ y val)})
    )
  )

(defn update-pos-and-aim [{:keys [aim x y]} cmd]
  (let [[dir val-s] (s/split cmd #" ")
        val (Integer/parseInt val-s)]
    (cond
      (= "forward" dir) {:aim aim :x (+ x val) :y (+ y (* aim val))}
      (= "up" dir) {:aim (- aim val) :x x :y y}
      (= "down" dir) {:aim (+ aim val) :x x :y y})
    )
  )

(defn navigate [fn start infn]
  (let [cmds (s/split (slurp infn) #"\n")
        {:keys [aim x y]} (reduce fn start cmds)]
    (* x y)
    )
  )

