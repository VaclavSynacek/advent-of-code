(def small
  (slurp "small.txt"))

(def input
  (slurp "input.txt"))


(defn parse [raw]
  (->> raw
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" "))
       (map #(repeat (read-string (second %)) (first %)))
       flatten
       (map keyword)))

;(parse small)

(defn move-head [[x y] direction]
  (case direction
    :U [x (+ y 1)]
    :D [x (- y 1)]
    :L [(- x 1) y]
    :R [(+ x 1) y]))

(defn is-close? [[x1 y1] [x2 y2]]
  (and
    (< (abs (- x1 x2)) 2)
    (< (abs (- y1 y2)) 2)))

(defn pull [[x1 y1] [x2 y2]]
  [(cond
     (< x1 x2) (+ x1 1)
     (> x1 x2) (- x1 1)
     "equal" x1)
   (cond
     (< y1 y2) (+ y1 1)
     (> y1 y2) (- y1 1)
     "equal" y1)])

(def initial-world
  {:head [0 0]
   :tail [0 0]
   :been-to [[0 0]]})

(defn eval-one [world com]
  (let
    [head (if (keyword? com)
            (move-head (:head world) com)
            com)
     tail (if (is-close? head (:tail world))
            (:tail world)
            (pull (:tail world) head))
     been-to (cons tail (:been-to world))]
    {:head head
     :tail tail
     :been-to been-to}))

(defn eval-all [world commands]
  (reduce eval-one world commands))

;; first half
(->> input
  parse
  (eval-all initial-world)
  :been-to
  distinct
  count)

;; ----------------------------------------------


(defn world-to-next-commands [world]
  (reverse (:been-to world)))

(defn one-world-deeper [[world commands]]
  [initial-world (world-to-next-commands (eval-all world commands))])

;; second half
(->> (nth
       (iterate one-world-deeper [initial-world (parse input)])
       9)
     second
     distinct
     count)

