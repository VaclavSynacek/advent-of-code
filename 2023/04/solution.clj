(def small
  (->> (slurp "small-input.txt")
    (clojure.string/split-lines)))

(def input
  (->> (slurp "input.txt")
    (clojure.string/split-lines)))


(defn parse-line [text]
  (let
    [whole (clojure.string/split text #":")
     data (clojure.string/split (second whole) #"\|")]
    {:win (read-string (str "#{" (first data) "}"))
     :my (read-string (str "[" (second data) "]"))}))

(defn my-winning [{:keys [win my]}]
  (filter win my))

(require '[clojure.math :as math])

(defn rate [winning]
  (->> winning
    count
    dec
    (clojure.math/pow 2)
    (clojure.math/floor)))


(->> small
  (map parse-line)
  (map my-winning)
  (map rate)
  (reduce +))

(->> input
  (map parse-line)
  (map my-winning)
  (map rate)
  (reduce +))

;; ---------------------------------

(defn to-strut [input]
  (let
    [wins
      (->> input
        (map parse-line)
        (map my-winning)
        (map count))]
    {:wins wins
     :copies (map (constantly 1) wins)
     :total 0}))


(to-strut small)


(defn do-one-step [world]
  (let
    [this-wins (first (:wins world))
     this-copies (first (:copies world))
     wins (rest (:wins world))
     extra-copies (concat
                    (map (constantly this-copies) (range this-wins))
                    (repeat 0))
     copies (map +
                 (rest (:copies world))
                 extra-copies)
     total (+ (:total world) this-copies)]
    {:wins wins
     :copies copies
     :total total}))


(->> (iterate do-one-step (to-strut input))
  (drop-while #(> (count (:wins %)) 0))
  first
  :total)
