(def small (slurp "small.txt"))

(def input (slurp "input.txt"))

(defn parse [in]
  (->> in
    (clojure.string/split-lines)
    (map #(map (comp read-string str) %))))

;(parse small)

(defn visible-in-line [line]
  (->> line
    (map-indexed
      (fn [idx val]
        [val (take (+ idx 1) (cons -1 line))]))
    (map
      (fn [[tree trees]]
        (> tree (reduce max trees))))))

; (visible-in-line [2 3 4 0 2])

(defn visible-from-left [forest]
  (->> forest
    (map visible-in-line)))

;(visible-from-left (parse small))

(visible-from-left (parse small))

(defn nth-column [matrix n]
  (for [row matrix] (nth row n)))

(defn rot90 [matrix]
  (for [column (reverse (range (count (first matrix))))]
    (nth-column matrix column)))

(defn rot90-n-times [times matrix]
  (nth 
    (iterate rot90 matrix)
    times))
             
(def all-rotations
  (map #(identity [% (- 4 %)]) (range 4)))
             
(defn visible-from-all-sides [matrix]
  (map
    (fn [[r1 r2]]
       (->> matrix
            (rot90-n-times r1)
            (visible-from-left)
            (rot90-n-times r2)))
    all-rotations))

;(visible-from-all-sides (parse small))

(defn matrix-map [f m1 m2]
  (map
    (fn [r1 r2]
      (map f r1 r2))
    m1 m2))

(defn visible-all-around [matrix]
  (reduce (partial matrix-map #(or %1 %2)) (visible-from-all-sides matrix)))

;; first half
(->> input
     parse
     visible-all-around
     flatten
     (filter identity)
     count)


;; --------------------------------------------

(defn sight-in-line [line]
  (->> line
    (map-indexed
      (fn [idx val]
        [val (reverse (take idx line))]))
    (map
      (fn [[tree trees]]
        (take (->> trees
               (take-while #(< % tree))
               count
               (+ 1))
              trees)))
    (map count)))

(defn sight-from-left [forest]
  (->> forest
    (map sight-in-line)))


(defn sight-from-all-sides [matrix]
  (map
    (fn [[r1 r2]]
       (->> matrix
            (rot90-n-times r1)
            (sight-from-left)
            (rot90-n-times r2)))
    all-rotations))

(defn sight-all-around [matrix]
  (reduce (partial matrix-map #(* %1 %2)) (sight-from-all-sides matrix)))

;; second half
(->> input
     parse
     sight-all-around
     flatten
     (apply max))
