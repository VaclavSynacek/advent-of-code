(def small (slurp "small.txt"))

(def input (slurp "input.txt"))

(use 'clojure.string)
(use 'clojure.pprint)

(defn parse-to-raw [in]
  (->> in
    (split-lines)
    (mapv #(into [] (seq (char-array %))))))

(parse-to-raw small)

(defn find-all [matrix v]
  (for [x (range (count matrix))
        y (range (count (first matrix)))
        :when (= v
                 (nth (nth matrix x) y))]
       [x y]))

(find-all
  (parse-to-raw small)
  \E)

(defn mset [matrix cords v]
  (assoc-in matrix cords v))

(defn mset-if-not [matrix cords v]
  (if (nil? (get-in matrix cords))
    (assoc-in matrix cords v)
    matrix))

(defn nilm [matrix]
  (clojure.walk/postwalk
    #(if (coll? %)
       %
       nil)
    matrix))

(defn parse-to-world [raw]
  (let
    [start (first (find-all raw \S))
     end   (first (find-all raw \E))
     ground (-> raw
                (mset start \a)
                (mset end \z))
     access (-> (nilm raw)
                (mset start 0))]
    {:ground ground
     :access access
     :end end}))

(def world (parse-to-world (parse-to-raw small)))

world

(defn next-positions [[x y]]
  [[(- x 1) y]
   [(+ x 1) y]
   [x (- y 1)]
   [x (+ y 1)]])

(defn in-matrix? [m [x y]]
  (and
    (<= 0 x (- (count m) 1))
    (<= 0 y (- (count (first m)) 1))))

(defn gradient-ok? [from to]
  (>= (- (int from) (int to)) -1))


(defn get-next [world [x y]]
  (let
    [cur (get-in world [:ground x y])]
    (->> (next-positions [x y])  
      (filter #(in-matrix? (:ground world) %))
      (filter (fn [[x y]]
                (gradient-ok?
                  cur
                  (get-in world [:ground x y])))))))



(defn step-after [world st]
  (let
    [prev (find-all (:access world) st)
     nexts (->> (map #(get-next world %) prev)
            (apply concat)
            (distinct))
     access (reduce (fn [acc pos]
                      (mset-if-not acc pos (+ 1 st)))
                    (:access world)
                    nexts)]
    (assoc-in world [:access] access)))

(defn step [world]
  (let
    [m (->> (:access world)
          (flatten)
          (filter identity)
          (apply max))]
    (step-after world m)))


; first half
(let
  [world (parse-to-world (parse-to-raw input))
   after (->> (iterate step world)
             (partition 2 1)
             (drop-while (fn [[a b]] (not= a b)))
             first
             first)
   loc (into [] (concat [:access] (:end after)))
   answer (get-in after loc)]
  answer)








