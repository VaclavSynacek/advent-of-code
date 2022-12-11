(def small (slurp "small.txt"))

(def input (slurp "input.txt"))

(use 'clojure.string)
(use 'clojure.pprint)

(defn last-n [st]
  (->> (split st #" ")
    last
    read-string))

(defn make-inspect-fn [st]
  (let
    [infix (->> (split st #"=")
            second
            trim
            (#(split % #" "))
            (map #(if (= "old" %) "%" %)))]
    (eval (read-string (str
                        "#("
                        (nth infix 1)
                        " "
                        (nth infix 0)
                        " "
                        (nth infix 2)
                        ")")))))

(defn parse-monkey [lines]
  {:items (->> (split (second lines) #"([\,:])")
            (rest)
            (map read-string)
            (map bigint))
   :inspect (make-inspect-fn (nth lines 2))
   :divisible (last-n (nth lines 3))
   :extra-modulo (read-string (last (split (nth lines 3) #" ")))
   :divisible-yes (last-n (nth lines 4))
   :divisible-no (last-n (nth lines 5))
   :inspected 0})

(defn parse [in]
  (->> (str in "\n"\n)
    (split-lines)
    (partition 7)
    (mapv parse-monkey)))

(defn divisible?
  [num div]
  (zero? (mod num div)))

(defn dont-worry [n]
  (Math/floor (/ n 3)))

(defn process-monkey [monkeys i]
  (let
    [cur-monkey (nth monkeys i)
     processed (->> (:items cur-monkey)
                  (map (:inspect cur-monkey))
                  (map dont-worry))
     inspected (count processed)
     throw-up (filter #(divisible? % (:divisible cur-monkey)) processed)
     throw-down (remove #(divisible? % (:divisible cur-monkey)) processed)]
    ;(println "cur-monkey (" i "): " cur-monkey)
    ;(println "processed: " processed)
    ;(println "will throw: " throw-up "///" throw-down)
    (-> monkeys
       (assoc-in [i :items] '())
       (update-in [i :inspected] + inspected)
       (update-in [(:divisible-yes cur-monkey) :items] concat throw-up)
       (update-in [(:divisible-no cur-monkey) :items] concat throw-down))))


(defn process-round [monkeys]
  (reduce process-monkey monkeys (range (count monkeys))))

(pprint (parse input))

(pprint (->> (parse input)
         (process-round)))

(pprint (nth (iterate process-round (parse input)) 20))

;; first half
(->> (nth (iterate process-round (parse input)) 20)
  (map :inspected)
  (sort >)
  (take 2)
  (apply *))

;;------------------------------------

;; yes, redefine existing fn :)
(defn dont-worry [n]
   n)

;; probably answer to second half, if I had a supercomputer
(->> (nth (iterate process-round (parse input)) 10000)
 (map :inspected)
 (sort >)
 (take 2)
 (apply *))


;; ------------------------------------
;; second half
;; unfortunatelly this optimization I had to google up

(defn supermodulo [monkeys]
  (apply * (distinct (concat (->> monkeys
                                (map :items)
                                (flatten))
                            (->> monkeys
                               (map :extra-modulo))))))

(def superm (supermodulo (parse input)))

(defn dont-worry [n]
  (mod n superm))

(->> (nth (iterate process-round (parse input)) 10000)
 (map :inspected)
 (sort >)
 (take 2)
 (apply *))
