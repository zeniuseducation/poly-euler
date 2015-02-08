(ns clein.one)

(def raw
  (->> (slurp "resources/p082_matrix.txt")
       (partition-by #{\newline})
       (remove #{[\newline]})
       (map #(apply str %))
       (map #(str "[" % "]"))
       (map read-string)))

(def num
  (memoize
   (fn [[x y]]
     (nth (nth raw y) x))))

(def all
  (memoize
   (fn [[x y]]
     (cond (= x 79)
           nil
           :else
           (cond (zero? y)
                 {:right [(inc x) y] :down [x 1]}
                 (= y 79)
                 {:right [(inc x) y] :up [x (dec y)]}
                 :else
                 {:right [(inc x) y] :up [x (dec y)] :down [x (inc y)]})))))

(def mmin
  (memoize
   (fn [[x y] mkey]
     (if-let [jalan (all [x y])]
       (+ (num [x y])
          (->> (dissoc jalan mkey)
               (map #(mmin (val %)
                           (condp = (key %)
                             :right :bebas
                             :up :down
                             :down :up
                             :else :bebas)))
               (apply min)))
       (num [x y])))))

(defn euler82
  [mat]
  (time (->> (map #(vector 0 %) (range 80))
             (pmap #(mmin % :down))
             (apply min))))




