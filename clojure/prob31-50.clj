(load-file "math.clj")

(defn elem?
  [n ls]
  (if (some #(= n %) ls) true false))

(defn add2
  [n]
  (+ 2 n))

(defn prime?
  "Using some laziness feature for checking prime"
  [n]
  (cond (< n 10) (elem? n [2 3 5 7])
        (even? n) false
        :else (->> (iterate add2 3)
                   (take-while #(<= (sqr %) n))
                   (every? #(not (div? n %))))))

(defn next-prime
  "Returns the next prime larger than n"
  [n]
  (cond (< n 2) 2
        (= n 2) 3
        (even? n) (if (prime? (inc n)) (inc n) (next-prime (inc n)))
        :else (first (drop-while #(not (prime? %)) (iterate add2 (add2 n))))))

(defn tprime?
  "Returns true if p is a truncatable prime"
  [p]
  (let [ncol (numcol p)
        lcol (take (count ncol) (iterate butlast ncol))
        rcol (take (count ncol) (iterate rest ncol))]
    (every? prime? (map colnum (concat lcol rcol)))))

(defn sol37
  "Returns the sum of n number of truncatable primes"
  [n]
  (->> (iterate next-prime 10)
       (filter tprime?)
       (take n)
       sum
       time))

(defn limb
  [a lim]
  (min (quot lim 2)
       (inc (quot (inc (sqr a)) 2))))


;; (inc (limb a lim))

(defn triplets
  "Returns the pythagorean triplets with perimeters less than lim"
  [lim]
  (for [a (range 3 (inc (quot lim 4)))
        b (range (inc a) (quot lim 2))
        :let [csqr (+ (sqr a) (sqr b))
              c (int (Math/sqrt csqr))]
        :when (psqr? csqr)]
    (+ a b c)))

(defn sol39
  [lim]
  (->> (triplets lim)
       frequencies
       (sort-by second)
       last
       time))

;; Problem 40
(defn sol40
  [lim]
  (->> (range (inc lim))
       (map #(expt 10 %))
       (map #(nth (mapcat numcol (iterate inc 1)) (dec %)))
       product
       time))











