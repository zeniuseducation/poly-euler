(ns clein.p83)

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

(def ways
  (fn [[x y]]
    (if (every? #{79} [x y])
      nil
      (->> #{[x (inc y)]
             [x (dec y)]
             [(inc x) y]
             [(dec x) y]}
           (remove #(or (neg? (first %))
                        (neg? (second %))
                        (>= (first %) 80)
                        (>= (second %) 80)))
           set))))

(def mmin
  (memoize
   (fn [[x y] visits]
     (if (> visits 200)
       99999999999
       (if-let [choices (ways [x y])]
         (+ (num [x y])
            (->> choices
                 (map #(mmin % (inc visits)))
                 (apply min)))
         (num [x y]))))))


(defn euler83
  []
  (time (mmin [0 0] 1)))


