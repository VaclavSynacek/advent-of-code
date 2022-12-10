(def xsmall
  "noop
addx 3
addx -5")


(def small (slurp "small.txt"))

(def input (slurp "input.txt"))


(defn parse [data]
  (->> data
    (clojure.string/split-lines)
    (map #(clojure.string/split % #" "))
    (map #(identity [(keyword (first %)) (when (second %)
                                            (read-string (second %)))]))))


(defn process-one [world command]
  (let
    [prev (last world)]
    (concat world (case (first command)
                    :noop [prev]
                    :addx [prev (+ prev (second command))]))))

(process-one [1] (first (parse small)))

(defn process-all [world commands]
  (reduce process-one world commands))

;; first half
(->> input
  parse
  (process-all [:pl 1])
  (map-indexed (fn [idx v] (identity [idx v])))
  (filter #((set (range 20 400 40)) (first %)))
  (map #(apply * %))
  (apply +))

;-----------------------------------------

(defn sprite-pixels [pos]
  (set
    [
     pos
     (+ pos 1)
     (+ pos 2)]))

(comment
  ;; oops, not like this
  (->> small
    parse
    (process-all [:pl 1])
    (map-indexed (fn [idx v] (identity [idx v])))
    (drop 1)
    (map
      (fn [[cyc spri]]
        (if ((sprite-pixels spri) cyc)
          "#"
          ".")))
    (partition 40)
    (map #(apply str %))))

; second half after a bit of line reorderings
(->> input
  parse
  (process-all [:pl 1])
  (drop 1)
  (partition 40)
  (map #(map-indexed (fn [idx v] (identity [(+ 1 idx) v])) %))
  (map #(map
          (fn [[cyc spri]]
            (if ((sprite-pixels spri) cyc)
              "#"
              "."))
          %))
  (map #(apply str %))
  (clojure.string/join "\n")
  (println))
