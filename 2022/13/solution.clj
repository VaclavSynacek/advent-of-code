(def small (slurp "small.txt"))
(def input (slurp "input.txt"))

(use 'clojure.string)
(use 'clojure.pprint)


(defn parse [in]
  (->> in
    (#(str % "\n"))
    (#(split % #"\n\n"))
    (map split-lines)
    (map #(map read-string %))))

(pprint (parse small))

(pprint (parse input))

(defn class2 [x y]
  [(class x) (class y)])

(defmulti compare-packets class2)

(defmethod compare-packets [java.lang.Long java.lang.Long] [left right]
  (println "compare numbers: " left " vs " right)
  (compare left right))

(defmethod compare-packets
  [clojure.lang.PersistentVector clojure.lang.PersistentVector] [left right]
  (println "compare vectors " left " vs " right)
  (let
    [result (->> (map compare-packets left right)
              (remove #(= % 0))
              first)]
    (if (= nil result)
      (compare (count left) (count right))
      result)))
      

(defmethod compare-packets [clojure.lang.PersistentVector java.lang.Long] [left right]
  (println "compare mixed " left " vs " right)
  (compare-packets left [right]))

(defmethod compare-packets [java.lang.Long clojure.lang.PersistentVector] [left right]
  (println "compare mixed " left " vs " right)
  (compare-packets [left] right))

(defn top-compare [pair]
  (println "top compare: " (first pair) " /// " (second pair))
  (if (neg? (compare-packets (first pair) (second pair)))
    true
    false))

; first half
(let
  [results (->> (parse input)
            (map top-compare))
   indexes (range 1 (+ 1 (count results)))]
  (->> (map vector indexes results)
    (filter #(second %))
    (map first)
    (reduce +)))
   

