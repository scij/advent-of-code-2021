(ns advent-of-code-2021.day10
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def open #{\( \{ \[ \<})

(def paren-value-1 {
                    \) 3
                    \] 57
                    \} 1197
                    \> 25137
                    })

(def paren-value-2 {
                    \( 1
                    \[ 2
                    \{ 3
                    \< 4
                    })

(defn matches-paren [open-p close-p]
  (or
    (and (= open-p \() (= close-p \)))
    (and (= open-p \{) (= close-p \}))
    (and (= open-p \[) (= close-p \]))
    (and (= open-p \<) (= close-p \>)))
  )

(defn parse-line-1 [l stack]
  (if (empty? l)
    0
    (if (contains? open (first l))
      (parse-line-1 (rest l) (conj stack (first l)))
      (if (matches-paren (peek stack) (first l))
        (parse-line-1 (rest l) (pop stack))
        (get paren-value-1 (first l))
        )
      )
    )
  )

(defn parse-file [fn]
  (with-open [rdr (clojure.java.io/reader fn)]
    (reduce +
            (map #(parse-line-1 % '())
                 (line-seq rdr)))
    )
  )

(defn value-of-stack [stack]
  (reduce (fn [total score]
            (+ score (* total 5)))
          0
          (map (fn [paren]
                 (get paren-value-2 paren))
               stack)
          )
  )

(defn parse-line-2 [l stack]
  (if (empty? l)
    (value-of-stack stack)
    (if (contains? open (first l))
      (parse-line-2 (rest l) (conj stack (first l)))
      (if (matches-paren (peek stack) (first l))
        (parse-line-2 (rest l) (pop stack))
        0)
      )
    )
  )

(defn parse-file-2 [fn]
  (with-open [rdr (clojure.java.io/reader fn)]
    (sort (filter pos?
                  (map #(parse-line-2 % '())
                       (line-seq rdr))))
    )
  )
