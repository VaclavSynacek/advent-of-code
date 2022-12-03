(defn split-all [pred items]
        (when (seq items)
          (apply conj (reduce (fn [[acc curr] x]
                                (if (pred x)
                                  [(conj acc curr) []]
                                  [acc (conj curr x)]))
                              [[] []] items))))


(def small
  (slurp "small-input.txt"))

(def input
  (slurp "input.txt"))

(defn one-elf [bag]
  (->> bag
       (map read-string)
       (reduce +)))

(defn totals [input]
  (->> input
    (clojure.string/split-lines)
    (split-all #(= "" %))
    (map one-elf)))

;; first answer
(apply max (totals input))

;; second answer
(->> input
     totals
     sort
     reverse
     (take 3)
     (reduce +))
