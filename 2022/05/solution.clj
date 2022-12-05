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
    string-or-tree))o

(defn parse-command [line]
  (->> (clojure.string/split line #" ")
    (map-indexed (fn [idx itm]
                   (if (even? idx)
                     (keyword itm)
                     (read-string itm))))
    (apply hash-map)))


(defn parse [in]
  (let
    [lines (clojure.string/split-lines in)
     sep (split-with (partial not= "") lines)
     containers-raw (drop-last (first sep))
     num-heaps (/ (+ 1 (count (last (first sep)))) 4)
     moves (->> (drop 1 (second sep))
              (map parse-command))
     heaps (->> (for [i (range num-heaps)]
                  (for [s (range (count containers-raw))]
                    (nth (nth containers-raw s) (+ 1 (* 4 i)))))
              (map #(remove #{\space} %))
              (into []))]
    {:heaps heaps
     :moves moves}))


(defn move1 [heaps {:keys [from to]}]
  ;(println "heaps:" heaps)
  ;(println "move from: " from " to: " to)
  (let
    [from (- from 1)
     to (- to 1)
     cont (first (nth heaps from))]
    (-> heaps
      (update-in [from] #(drop 1 %))
      (update-in [to] #(cons cont %)))))


(defn move [heaps m]
  (reduce move1 heaps (take (:move m) (repeat m))))

(defn process-all-moves [{:keys [heaps moves]}]
  (reduce move heaps moves))

;; first half
(->> (parse input)
  (process-all-moves)
  (map first))

(defn rovnak-na-vohejbak [world]
  "Just for fun use CrateMover9000 to simulate CrateMover9001; do not try at home or in real world with real Elves"
  (let
    [tmp (+ 1 (count (:heaps world)))]
    (-> world
        (update-in [:heaps] conj '()) 
        (update-in [:moves] (fn [moves]
                              (apply concat
                                (map
                                  (fn [{:keys [move from to]}]
                                     [{:move move
                                       :from from
                                       :to tmp}
                                      {:move move
                                       :from tmp
                                       :to to}])
                                  moves)))))))

;; second half
(->> (parse input)
  rovnak-na-vohejbak
  (process-all-moves)
  (map first)
  (drop-last))
