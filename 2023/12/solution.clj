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
  (let
    [result (->> recs
              generate
              (filter #(valid? % checks))
              count)]
    (println "GENERATED one RESULT")
    result))


(time (->> small
       (map get-num-of-possibilities)
       (reduce +)))

(time (->> input
       (pmap get-num-of-possibilities)
       (reduce +)))

;; ------------------------------------


(defn unfold-one [{:keys [recs checks]}]
  {:recs (str recs "?" recs "?" recs "?" recs "?" recs)
   :checks (concat checks checks checks checks checks)})

(comment "probably would produces result, but not on today hw :)"
  (->> small
    (map unfold-one)
    (pmap get-num-of-possibilities)
    (reduce +)))

(comment "5 times faster, but still does not finish with input data"
  (defn start-valid? [text checks]
    (let
      [start-checks (butlast (checksums text))
       trim-checks (take (count start-checks) checks)
       result (or (nil? start-checks)
                (= start-checks trim-checks))]
      result))

  (defn generate-valid [[base checks]]
    (loop
      [base base
       results [""]]
      (if (seq base)
        (let
          [the-one (first base)]
          (if ((set options) the-one)
            (recur
              (rest base)
              (->> (map #(str % the-one) results)
                (filter #(start-valid? % checks))))
            (recur
              (rest base)
              (->> (append-options results)
                (filter #(start-valid? % checks))))))
        (do
          (let
            [final
              (->> results
               (filter #(valid? % checks))
               count)]
            (println "one done with result: " final)
            final)))))


  (time (->> small
         (map unfold-one)
         (map (fn [m] [(:recs m) (:checks m)]))
         (pmap generate-valid)
         (reduce +)))

  (time (->> input
         (map unfold-one)
         (map (fn [m] [(:recs m) (:checks m)]))
         (pmap generate-valid)
         (reduce +))))

