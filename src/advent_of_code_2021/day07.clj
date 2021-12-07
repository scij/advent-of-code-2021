(ns advent-of-code-2021.day07)

(defn fuel-burn-1 [a b]
  (Math/abs ^int (- a b)))

(defn fuel-burn-2 [a b]
  (let [n (Math/abs ^int (- a b))]
    (/ (+ (* n n) n) 2)
    )
  )

(defn diff-to-posn [fn posn t]
  (apply + (map #(fn t %) posn)))

(defn common-path [fn posn]
  (let [lwb (apply min posn)
        upb (apply max posn)]
    (loop [t lwb
           diff (diff-to-posn fn posn t)
           best-diff diff
           best-t t]
      (if (> t upb)
        best-diff
        (if (< diff best-diff)
          (recur (inc t) (diff-to-posn fn posn (inc t)) diff t)
          (recur (inc t) (diff-to-posn fn posn (inc t)) best-diff best-t)
          )
        )
      )
    )
  )