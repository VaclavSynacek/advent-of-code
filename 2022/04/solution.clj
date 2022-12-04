(def small
  (slurp "small-input.txt"))

(def input
  (slurp "input.txt"))

small

input

(defn deep-string-change [f string-or-tree]
  (clojure.walk/postwalk
    (fn [string-or-tree]
      (str "now in " string-or-tree)
      (if (string? string-or-tree)
        (f string-or-tree) 
        string-or-tree))
    string-or-tree))


;; first half
(->> small
  clojure.string/split-lines
  (map #(clojure.string/split % #",")))

(defn inside> [[[a b] [c d]]]
  (and
    (<= a c)
    (>= b d)))

(defn inside [[p1 p2]]
  (or
    (inside> [p1 p2])
    (inside> [p2 p1])))


;; first half
(->> input
  clojure.string/split-lines
  (deep-string-change #(clojure.string/split % #","))
  (deep-string-change #(clojure.string/split % #"-"))
  (deep-string-change read-string)
  (map inside)
  (filter identity)
  (count))


(defn overlap> [[[a b] [c d]]]
  (or
    (<= a c b)
    (<= a d b)))

(defn overlap [[p1 p2]]
  (or
    (overlap> [p1 p2])
    (overlap> [p2 p1])))




;; second half
(->> input
  clojure.string/split-lines
  (deep-string-change #(clojure.string/split % #","))
  (deep-string-change #(clojure.string/split % #"-"))
  (deep-string-change read-string)
  (map overlap)
  (filter identity)
  (count))





