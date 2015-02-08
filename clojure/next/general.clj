(ns poly.general)

(set! *unchecked-math* true)

(def palin?
  (fn [x]
    (let [bahan (str x)]
      (= bahan (->> bahan reverse (apply str))))))

(def euler5
  (memoize
   (fn [^long a ^long b]
     (let [tmp (int (* a b))]
       (if (palin? tmp)
         tmp
         (if (< tmp 800000)
           0
           (max (euler5 a (dec b))
                (euler5 (dec a) b))))))))

(defn euler5b
  [^long start ^long end]
  (apply max
         (for [i (range start end -1)
               j (range i end -1)
               :let [tmp (int (* i j))]
               :when (palin? tmp)]
           tmp)))

(defn qsort1
  [[x & xs]]
  (if xs
    (concat (qsort1 (filter #(<= % x) xs))
            [x]
            (qsort1 (filter #(> % x) xs)))
    (if x [x] [])))

(defn qsort
  [[x & xs]]
  (if xs
    (let [small (filter #(<= % x) xs)]
      (concat (qsort small) [x] (qsort (remove (set small) xs))))
    (if x [x] [])))

(defn qsort2
  [[x & xs]]
  (cond xs (let [bahan (group-by #(<= % x) xs)
                 smaller (bahan true)
                 larger (bahan false)]
             (concat (qsort2 smaller) [x] (qsort2 larger)))
        x [x]
        :else []))

(defn msort
  [[l & ls]]
  (if l
    (if ls
      (let [merge-part
            (fn merge-part [xs ys]
              (if xs
                (if ys
                  (let [[x & xxs] xs [y & yys] ys]
                    (if (<= x y)
                      (cons x (merge-part xxs ys))
                      (cons y (merge-part xs yys))))
                  xs)
                ys))
            ls1 (cons l ls)
            part (quot (count ls1) 2)]
        (merge-part (msort (take part ls1))
                    (msort (drop part ls1))))
      [l])
    []))



