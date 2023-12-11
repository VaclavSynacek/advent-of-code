(def small
  (->> (slurp "small-input.txt")
    (clojure.string/split-lines)))

(def input
  (->> (slurp "input.txt")
    (clojure.string/split-lines)))


(defn line->data [line]
  (let
    [numbers (->> line
               (map int)
               (keep #(when (<= 48 % 57)
                        %))
               (map char))
     data (parse-long (str (first numbers) (last numbers)))]
    data))


(->> small
     (map line->data)
     (reduce +))
  
(->> input
     (map line->data)
     (reduce +))

;; --------------------------------------------------


(def words
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn starts-with-word-number? [text]
  (->> (map
         (fn [[key value]]
            (when (clojure.string/starts-with? text key)
              value))
         words)
    (remove nil?)
    first))

(defn starts-with-digit? [text]
  (when (<= 48 (int (first text)) 57)
    (parse-long (str (first text)))))

(defn to-number [text]
  (or (starts-with-digit? text)
    (starts-with-word-number? text)))


(defn line->numbers [text]
  (->> (for
         [i (range 0 (count text))]
         (to-number (subs text i)))
    (remove nil?)))

(defn line->data [text]
  (let
    [numbers (line->numbers text)]
    (+
      (* 10 (first numbers))
      (last numbers))))

(->> input
     (map line->data)
     (reduce +))
