(ns euler.prob82)

(load-file "math.clj")

(defn adhoc82
  [fname]
  (let [sf (slurp fname)
        tmp (str "[" sf "]")]
    (do (spit "brako.txt" tmp)
        (->> (read-string tmp)
             (partition 80)
             (map #(cons 9999999 %))
             (cons (repeat 81 9999999))
             (reverse)
             (cons (repeat 81 9999999))
             (reverse)))))

(defn getcell
  [[a b]]
  (nth (nth bahan b) a))

(def bahan (adhoc82 "matrix.txt"))

(defn up [[a b]] [a (dec b)])
(defn down [[a b]] [a (inc b)])
(defn right [[a b]] [(inc a) b])

(defn base
  [[a b] bahan]
  (->> (range a 81)
       (map #(getcell [% b]))
       sum))

(defn ups
  [cell bahan]
  [:up (getcell (up cell))])

(defn downs
  [cell bahan]
  [:down (getcell (down cell))])

(defn rights
  [cell bahan]
  [:right (getcell (right cell))])

(def moves {:right rights :up ups :down downs})
(def bmoves {:right right :up up :down down})

(defn cpath
  [cell]
  (first (sort-by second (map #((val %) cell bahan) moves))))

(defn cmove
  [cell]
  (let [move (cpath cell)]
    (((first move) bmoves) cell)))

(defn rpath
  [[a b]]
  (if (= 80 a)
    (getcell [a b])
    (if (= 79 a)
      (if (= 80 b)
        (+ (getcell [a b])
           (min (getcell (right [a b]))
                (getcell (up [a b]))))
        (if (= 1 b)
          (+ (getcell [a b])
             (min (getcell (right [a b]))
                  (getcell (down [a b]))))
          (+ (getcell [a b])
             (min (rpath (up [a b]))
                  (rpath (down [a b]))
                  (getcell (right [a b]))))))
      )))




