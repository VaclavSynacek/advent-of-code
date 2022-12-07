(def small-file "small.txt")
(def input-file "input.txt")

(defn transform-ls [pair]
  (if (= "dir" (first pair))
    [:create-dir {:name (second pair)}]
    [:create-file {:name (second pair) :size (read-string (first pair))}]))

(defn transform-command [com]
  (case (first com)
    "cd" (case (second com)
           "/"  [[:cd-root]]
           ".." [[:cd-up]]
           [[:cd (second com)]])
    "ls" (map transform-ls (partition 2 (drop 1 com)))
    (.Exeption "UNKNOWN: " com)))
    


(defn parse-commands [file]
  (as-> file <>
    (slurp <>)
    (str "\n" <>)
    (clojure.string/split <> #"\n\$ ")
    (drop 1 <>)
    (map #(clojure.string/split % #"\s+") <>)
    (map transform-command <>)
    (apply concat <>)))


(parse-commands small-file)

(use 'clojure.zip)

(defn make-world [blueprint]
  (zipper
    (fn [node] (= :dir (:type node)))
    (fn [node] (:children node))
    (fn [node children]
        (assoc-in node [:children] children))
    blueprint))

(def empty-world
  (make-world  {:type :dir :name "/"}))

empty-world

(defmulti eval-command
  (fn [world com] (first com)))

(defmethod eval-command :default [world com]
  (println "Unknown command: " com)
  world)

(defmethod eval-command :cd-root [world com]
  (make-world (root world)))

(defmethod eval-command :cd-up [world com]
  (up world))

(defn find-dir [world dirname]
  (if (= dirname (:name (node world)))
    world
    (find-dir (right world) dirname)))

(defmethod eval-command :cd [world com]
  (find-dir (down world) (second com)))

(defmethod eval-command :create-dir [world com]
  (insert-child world {:type :dir :name (:name (second com))}))

(defmethod eval-command :create-file [world com]
  (insert-child world {:type :file
                       :name (:name (second com))
                       :size (:size (second com))}))

(defn get-filesystem [file]
  (->> file
    parse-commands
    (reduce eval-command empty-world)
    root))

(defn calculate-dirs [filesystem]
  (clojure.walk/postwalk
    (fn [node]
      (if (= :dir (:type node))
        (assoc node :size (reduce + (map :size (:children node))))
        node))
    filesystem))


;(calculate-dirs (get-filesystem small-file))

(defn get-dir-sizes [filesystem]
  (let
    [dirsizes (transient [])]
    (clojure.walk/postwalk
      (fn [node]
        (if (= :dir (:type node))
          (do
            (conj! dirsizes (:size node))
            node)
          node))
      filesystem)
    (persistent! dirsizes)))

;; first half
(->> input-file
  get-filesystem
  calculate-dirs
  get-dir-sizes
  (filter #(< % 100000))
  (reduce +))

;; second half
(let
  [filesystem
    (->> input-file
       get-filesystem
       calculate-dirs)
   root-size (:size filesystem)
   need (* -1 (- 40000000 root-size))
   dir-sizes (get-dir-sizes filesystem)]
   
  (->> dir-sizes
    (filter #(> % need))
    (apply min)))
