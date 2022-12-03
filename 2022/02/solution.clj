
(def small
  (slurp "small-input.txt"))

(def input
  (slurp "input.txt"))


small

input

(defn normalize [letter]
  ({:A :rock
    :B :paper
    :C :scissors
    :X :rock
    :Y :paper
    :Z :scissors} letter))

(def second-win-combinations
  #{[:rock :paper]
    [:paper :scissors]
    [:scissors :rock]})


(defn second-wins [[a b]]
  (cond
    (= a b) 3
    (second-win-combinations [a b]) 6
    "default" 0))

(defn bonus [[_ b]]
  ({:rock 1
    :paper 2
    :scissors 3} b))

(bonus [:rock :rock])


(defn rate [pair]
  (+
    (second-wins pair)
    (bonus pair)))

;; first half answer
(->> input
  clojure.string/split-lines
  (map #(clojure.string/split % #" "))
  (map #(map (comp normalize keyword) %))
  (map rate)
  (reduce +))

(def to-win
  {:rock :paper
   :paper :scissors
   :scissors :rock})

(def to-loose
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(defn decide [[a b]]
  [a
   (case b
     :X (to-loose a)
     :Y a
     :Z (to-win a))])

;; second answer
(->> input
  clojure.string/split-lines
  (map #(clojure.string/split % #" "))
  (map #(map keyword %))
  (map (fn [[a b]] [(normalize a) b]))
  (map decide)
  (map rate)
  (reduce +))

