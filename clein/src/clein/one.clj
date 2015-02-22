(ns clein.one)

(set! *unchecked-math* true)

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


(def modex
  (memoize
   (fn [^long a ^long b ^long div]
     (cond
      (== b 0) 1
      (== b 1) (rem a div)
      (even? b) (let [tmp (modex a (quot b 2) div)]
                  (rem (* tmp tmp) div))
      :else (rem (* (rem (modex a (quot b 2) div) div)
                    (rem (modex a (inc (quot b 2)) div) div)) div)))))

(defn hexp
  [^long a ^long b ^long div]
  (if (== b 1)
    (rem a div)
    (modex a (hexp a (dec b) div) div)))

(defn selipin
  [elmt xs n]
  (concat (take (dec n) xs)
          [elmt]
          (drop (dec n) xs)))

(defn prize?
  [xs]
  (if (< (count xs) 3)
    true
    (if (every? #{:a} (take 3 xs))
     false true)))

(defn cards
  [^long n]
  (loop [i (int 0) res ['()] counter []]
    (if (== i n)
      (concat res counter)
      (recur (inc i)
             (sequence
              (comp (mapcat #(list (cons :a %)
                                   (cons :o %)))
                    (filter prize?))
              res)
             (if (== i (dec n))
               (mapcat #(map (fn [x] (selipin :l % x))
                             (range 1 (inc n)))
                       res)
               counter)))))

(defn won?
  [xs]
  (if (< (count xs) 3)
    (if (> (count (filter #{:l} xs)) 1)
      false
      true)
    (if (every? #{:a} (take 3 xs))
      false
      (if (> (count (filter #{:l} xs)) 1)
        false
        true))))

(defn strings
  [^long n]
  (loop [i (int 0) res ['()]]
    (if (== i n)
      res
      (recur (inc i)
             (sequence
              (comp (mapcat #(list (cons :a %)
                                   (cons :o %)
                                   (cons :l %)))
                    (filter won?))
              res)))))


(def pentagonals
  "Generate an infinite lazy sequence of pentagonal numbers" 
  (pmap #(quot (* % (- (* 3 %) 1)) 2) (iterate inc 1)))

(defn ^boolean hexal?
  "Check for hexagonality"
  [^long n]
  (let [res (/ (+ 1 (Math/sqrt (+ 1 (* 8 n)))) 4)]
    (== (int res) res)))


(defn ^longs euler45
  [^long n]
  (->> pentagonals
       (filter hexal?)
       (take n)
       time))

(defn ^longs euler45t
  "Generates pentagonal numbers, filter that are also hexagonal, take 3"
  [^long n]
  (last
   (sequence
    (comp (map #(quot (* % (- (* 3 %) 1)) 2))
          (filter hexal?)
          (take n)) (range))))















