(def small "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def small2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(def small5 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(def input (slurp "input.txt"))

(defn packet? [col]
  (->> col
    frequencies
    vals
    (every? (partial = 1))))

(packet? '(\a \b \c \a))

(packet? '(\a \b \c \d))

;; first half
(->> input
     (partition 4 1)
     (map-indexed (fn [idx v]
                     (when (packet? v)
                       (+ 1 idx))))
     (drop-while nil?)
     (first)
     (+ 3))

;; seconf half
(->> input
     (partition 14 1)
     (map-indexed (fn [idx v]
                     (when (packet? v)
                       (+ 1 idx))))
     (drop-while nil?)
     (first)
     (+ 13))


