(def small
  (->> (slurp "small-input.txt")
    (clojure.string/split-lines)))

(def input
  (->> (slurp "input.txt")
    (clojure.string/split-lines)))


(defn print-it [universe]
  (doseq [l universe]
    (println l)))


(print-it small)

(print-it input)


(defn empty-line? [line]
  (every? #(= \. %) line))

(defn expand-lines [input]
  (remove nil? (mapcat
                 (fn [line]
                   [line
                    (when (empty-line? line)
                      line)])
                 input)))

(print-it (expand-lines small))


(defn transpose [universe]
  (apply map str universe))


(print-it (transpose small))

(defn expand-universe [universe]
  (->> universe
    expand-lines
    transpose
    expand-lines
    transpose))


(print-it (expand-universe small))


(defn get-galaxies [universe]
  (->> (map-indexed
        (fn [i row]
          (map-indexed
            (fn [j ch]
              (when (= ch \#)
                [i j]))
            row))
        universe)
     (apply concat)
     (remove nil?)))

(defn pairs [galaxies]
  (for [a galaxies
        b galaxies
        :while (not= a b)]
       [a b]))


(pairs [:a :b :c :d :e])

(pairs (get-galaxies small))

(defn distance [[[x1 y1] [x2 y2]]]
  (+
    (abs (- x1 x2))
    (abs (- y1 y2))))

(distance [[0 0] [1 1]])


(->> small
  expand-universe
  get-galaxies
  pairs
  (map distance)
  (reduce +))

(->> input
  expand-universe
  get-galaxies
  pairs
  (map distance)
  (reduce +))


;; -------------------------------------------------


(get-galaxies small)

(print-it small)

(defn empty-lines [lines]
  (->> (map-indexed
        (fn [i line]
          (when (empty-line? line)
            i))
        lines)
     (remove nil?)))

(defn between? [a b c]
  (or
    (< a b c)
    (> a b c)))

(let
  [
   ;input small
   input input
   ;expand-by 1
   expand-by 999999 
   rows (empty-lines input)
   cols (->> input
          transpose
          empty-lines)
   galaxies (get-galaxies input)
   extended-distance
    (fn [[[x1 y1] [x2 y2] :as all]]
      (+
        (distance all)
        (->> rows
          (filter #(between? x1 % x2))
          count
          (* expand-by))
        (->> cols
          (filter #(between? y1 % y2))
          count
          (* expand-by))))]
  (->> galaxies
    pairs
    (map extended-distance)
    (reduce +)))
    

