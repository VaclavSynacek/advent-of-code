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
