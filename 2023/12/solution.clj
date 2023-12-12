(defn parse-line [text]
  (let
    [parts (clojure.string/split text #" ")
     recs (first parts)
     checks (read-string (str "(" (second parts) ")"))]
    {:recs recs
     :checks checks}))

(def small
 (->> (slurp "small-input.txt")
   (clojure.string/split-lines)
   (map parse-line)))

(def input
  (->> (slurp "input.txt")
    (clojure.string/split-lines)
   (map parse-line)))

(def options [\. \#])


(defn append-options [strings]
  (for [o options
        s strings]
    (str s o)))

(defn generate [base]
  (loop
    [base base
     results [""]]
    (if (seq base)
      (let
        [the-one (first base)]
        (if ((set options) the-one)
          (recur
            (rest base)
            (map #(str % the-one) results))
          (recur
            (rest base)
            (append-options results))))
      results))) 

(generate "#??")


(defn checksums [text]
  (->> (clojure.string/split text #"\.")
    (remove #(= % ""))
    (map count)))


(defn valid? [text checks]
  (= checks
     (checksums text)))

(defn get-num-of-possibilities [{:keys [recs checks]}]
  (->> recs
    generate
    (filter #(valid? % checks))
    count))


(->> small
  (map get-num-of-possibilities)
  (reduce +))

(->> input
  (pmap get-num-of-possibilities)
  (reduce +))



