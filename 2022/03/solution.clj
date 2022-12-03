(require 'clojure.set)

(def small
  (slurp "small-input.txt"))

(def input
  (slurp "input.txt"))

(defn split-in-half [col]
  (let
    [len (count col)]
    (split-at (/ len 2) col)))

(defn prioritize [ch]
  (if (> (int ch) 95)
    (- (int ch) 96)
    (- (int ch) 38)))

;; first half
(->> input
  clojure.string/split-lines
  (map seq)
  (map split-in-half)
  (map #(map set %))
  (map #(apply clojure.set/intersection %))
  (map first)
  (map prioritize)
  (reduce +))

;; second half
(->> input
  clojure.string/split-lines
  (map seq)
  (partition 3)
  (map #(map set %))
  (map #(apply clojure.set/intersection %))
  (map first)
  (map prioritize)
  (reduce +))
