(ns alfa.under200.three
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

(defn repdash
  "Replacing dash in a string"
  [st]
  (cs/replace st #"-" "nil"))

(def graph
  (->> (slurp "resources/p107.txt")
       (split-lines)
       (map #(str "[" (repdash %) "]"))
       (mapv read-string)))

(defn intersect?
  [s1 s2]
  (let [inter (intersection s1 s2)]
    (if (empty? inter) false true)))

(def vertex
  (loop [i 0 res #{}]
    (if (>= i 40)
      res
      (recur (+ i 1)
             (->> (graph i)
                  (keep-indexed #(if %2 [%1 %2] %2))
                  (map #(vector #{i (first %)} (second %)))
                  set
                  (union res))))))

(def total-cost
  (->> vertex
       (map second)
       (reduce +)))

;; Problem 107 solved in 30ms
(defn min-tree
  [start lim]
  (loop [verti vertex verts #{start} tree #{}]
    (if (== lim (count verts))
      (- total-cost (->> (map second tree) (reduce +)))
      (let [bhn (->> verti
                     (filter #(and (intersect? verts (first %))
                                   (not-empty (difference (first %) verts))))
                     (min-by second))
            nvert (first (difference (first bhn) verts))]
        (recur (disj verti bhn)
               (conj verts nvert)
               (conj tree bhn))))))

(defn fst-expt
  [m]
  (let [f (fn f [^long a ^long b]
            (cond
              (== a b 1) 1
              (== a 1) (inc (apply min (map #(f % (- b %)) (range 1 (inc (quot b 2))))))
              (== b 1) (inc (apply min (map #(f % (- a %)) (range 1 (inc (quot a 2))))))
              (== a b) (inc (apply min (map #(f % (- a %)) (range 1 (inc (quot a 2))))))
              :else (+ (apply min (map #(f % (- a %)) (range 1 (inc (quot a 2)))))
                       (apply min (map #(f % (- b %)) (range 1 (inc (quot b 2))))))))]
    (apply min (map #(f % (- m %)) (range 1 (quot m 2))))))

(defn fexp1
  [m]
  (let [paths (make-array clojure.lang.PersistentHashSet (inc m))]
    (do (aset paths 1 #{})
        (aset paths 2 #{1})
        (loop [done #{1 2} stat false]
          (if (== m (count done))
            (if stat
              (rest (map-indexed #(vector %1 %2) (into [] paths)))
              (let [pos (for [i done j done
                              :let [ij (+ i j)]
                              :when (<= ij m)]
                          (if (== i j)
                            [ij (conj (aget paths i) i)]
                            [ij (conj (union (aget paths i)
                                             (aget paths j)) i j)]))
                    bhn (->> (group-by first pos)
                             (map #(vector (key %)
                                           (min-by count (map second (val %))))))]
                (do (doseq [[b1 b2] bhn]
                      (if-let [bget (aget paths b1)]
                        (if (> (count bget) (count b2))
                          (aset paths b1 b2))
                        (aset paths b1 b2)))
                    (recur (union done (set (map first bhn))) true))))
            (let [pos (for [i done j done
                            :let [ij (+ i j)]
                            :when (<= ij m)]
                        (if (== i j)
                          [ij (conj (aget paths i) i)]
                          [ij (conj (union (aget paths i)
                                           (aget paths j)) i j)]))
                  bhn (->> (group-by first pos)
                           (map #(vector (key %)
                                         (min-by count (map second (val %))))))]
              (do (doseq [[b1 b2] bhn]
                    (if-let [bget (aget paths b1)]
                      (if (> (count bget) (count b2))
                        (aset paths b1 b2))
                      (aset paths b1 b2)))
                  (recur (union done (set (map first bhn))) false))))))))

(defn tvals
  [node]
  (if (integer? node)
    [node]
    (set (cons (first node) (mapcat tvals (rest node))))))

(defn add-child
  [tree head child]
  (if (integer? tree)
    (if (= head tree)
      (vec (cons head child))
      tree)
    (if (= head (first tree))
      (vec (concat tree child))
      (vec (cons (first tree) (map #(add-child % head child) (rest tree)))))))

(defn shortest
  [tree node]
  (cond (integer? tree) (when (= tree node) [node])
        (= node (first tree)) [node]
        :else (let [mas (keep #(shortest % node) (rest tree))]
                (if (empty? mas)
                  nil
                  (cons (first tree) (min-by count mas))))))

(defn fexp2
  [m]
  (loop [tree [1] tval (tvals tree)]
    (if (= m (count tval))
      (->> (map #(vector % (shortest tree %)) tval)
           (sort-by first))
      (let [nxt (for [i tval j (shortest tree i)
                      :let [ij (+ i j)]
                      :when (<= ij m)]
                  [[i [j ij]] [j [i ij]]])
            ntree (reduce #(add-child %1 (first %2) (second %2))
                          tree
                          (apply concat nxt))]
        (recur ntree (tvals ntree))))))

(defn parent
  [tree node]
  (if (integer? tree)
    nil
    (if (= node (first tree))
      nil
      (if (some #{node} (map #(if (integer? %) % (first %)) (rest tree)))
        (first tree)
        (let [res (keep #(parent % node) (rest tree))]
          (if (empty? res) nil (first res)))))))

(defn all-parents
  [tree node]
  (->> (iterate #(parent tree %) node)
       (take-while identity)
       rest reverse))

(defn fexp
  [m]
  (let [st (->> (iterate #(* 2 %) 1)
                (take-while #(<= % (* 2 m))))
        stree (reduce #(add-child %1 (quot %2 2) [%2]) [1] st)]
    (loop [tree stree tval (tvals tree)]
      (if (<= (quot m 2) (count tval))
        (->> (map #(vector % (shortest tree %)) tval)
             (sort-by first))
        (let [nxt (for [cur tval i (shortest tree cur)
                        :let [nn (+ cur i)]
                        :when (<= nn m)]
                    [nn cur])
              ntree (reduce #(add-child %1 (second %2) [(first %1)])
                            tree
                            nxt)]
          (recur ntree
                 (tvals ntree)))))))


























