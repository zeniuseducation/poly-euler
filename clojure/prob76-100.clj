(ns euler.prob76-100)

(load-file "math.clj")

;; Problem 76

(defn partitions
  [m]
  (cond (= m 1) [1]
        :else (map #(conj (partitions (- m %)) %)
                   (range 1 m))))

(defn sol76
  [n]
  (sum (map #(if (even? %)
               (quot % 2)
               (inc (quot % 2))) (range 1 n))))

;; PROBLEM 85

(defn csrec
  [i j m n]
  (* (inc (- m i))
     (inc (- n j))))

(defn crec
  [m n]
  (sum (for [i (range 1 (inc m))
             j (range 1 (inc n))]
         (csrec i j m n))))

(defn sol85
  [n size]
  (time (->> (for [i (range 1 size)
                   j (range 1 size)]
               [(* i j) (abs (- n (crec i j)))])
             (sort-by second)
             (take 10))))

"Elapsed time: 1.5seconds"

;; PROBLEM 82

(defn adhoc82
  [fname]
  (let [sf (slurp fname)
        tmp (str "[" sf "]")]
    (do (spit "brako.txt" tmp)
        (->> (read-string tmp)
             (partition 80)
             (map #(cons 9999999 %))
             (cons (repeat 81 9999999))))))

(defn getcell
  [a b bahan]
  (nth (nth bahan b) a))

(def bahan (adhoc82 "matrix.txt"))

(defn up [[a b]] [a (dec b)])
(defn down [[a b]] [a (inc b)])
(defn right [[a b]] [(inc a) b])



