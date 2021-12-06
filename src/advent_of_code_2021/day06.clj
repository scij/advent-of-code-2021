(ns advent-of-code-2021.day06)

; naive approach
(defn countdown-fish [old-age]
  (if (zero? old-age)
    6
    (dec old-age))
  )

(defn new-fish [age]
  (zero? age))

(defn one-day [current-pop]
  (concat
    (map countdown-fish current-pop)
    (repeat (count (filter zero? current-pop)) 8))
  )

(defn for-days [day initial-pop]
  (if (zero? day)
    initial-pop
    (for-days (dec day) (one-day initial-pop)))
  )

; frequency based approach
(defn get-inital-population [population]
  (let [freq (frequencies population)]
    [(get freq 0 0)
     (get freq 1 0)
     (get freq 2 0)
     (get freq 3 0)
     (get freq 4 0)
     (get freq 5 0)
     (get freq 6 0)
     (get freq 7 0)
     (get freq 8 0)])
  )
(defn countdown-fish-2 [age-frequencies]
  [
   (get age-frequencies 1)
   (get age-frequencies 2)
   (get age-frequencies 3)
   (get age-frequencies 4)
   (get age-frequencies 5)
   (get age-frequencies 6)
   (+ (get age-frequencies 7) (get age-frequencies 0))
   (get age-frequencies 8)
   (get age-frequencies 0)]
  )

(defn for-days-2 [day pop-freq]
  (if (zero? day)
    pop-freq
    (for-days-2 (dec day) (countdown-fish-2 pop-freq)))
  )
