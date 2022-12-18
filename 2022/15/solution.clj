(def small (slurp "small.txt"))

(use 'clojure.string)
(use 'clojure.pprint)

(defn parse-line [l]
  (->>
    (split l #"[=,\,,:]")
    (map-indexed (fn [i v]
                   (when (odd? i)
                     v)))
    (filter identity)
    (mapv read-string)))

(defn manhatan [[x1 y1] [x2 y2]]
  (+
    (abs (- x1 x2))
    (abs (- y1 y2))))

(defn inside? [center distance point]
  (<= 
    (manhatan center point)
    distance))

(defn circles [in]
  (->> (split-lines in)
    (map parse-line)
    (mapv (fn [[a b c d]] [[a b] [c d]]))
    (mapv (fn [[p1 p2]] [p1 (manhatan p1 p2)]))))



