
(defn parse-color [text]
  (let
    [tokens (clojure.string/split text #" ")]
    {(keyword (nth tokens 2)) (read-string (nth tokens 1))}))

(defn parse-turn [text]
  (->> (clojure.string/split text #",")
    (map parse-color)
    (apply merge)))

(defn parse-turns [text]
  (->> (clojure.string/split text #";")
    (map parse-turn)
    vec))


(defn parse-line [text]
  (let
    [sp (clojure.string/split text #":")]
    {:id (read-string (subs (first sp) 4))
     :turns (parse-turns (second sp))}))


(def input
  (->> (slurp "input.txt")
    (clojure.string/split-lines)
    (map parse-line)))


(defn get-all-by-color [color turns]
  (->> (map color turns)
     (remove nil?)))
  

(defn valid-game? [{:keys [id turns]}]
  (when (and
         (<= (apply max (get-all-by-color :red turns)) 12)
         (<= (apply max (get-all-by-color :green turns)) 13)
         (<= (apply max (get-all-by-color :blue turns)) 14))
    id))


(->> input
  (map valid-game?)
  (remove nil?)
  (reduce +))
  



