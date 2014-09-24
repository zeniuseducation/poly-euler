(defn div?
  "Returns true if a is evenly divisible by m"
  [a m]
  (zero? (rem a m)))

(defn psqr?
  [n]
  (let [x (Math/sqrt n)]
    (== (int x) x)))

(defn sum
  "Accepts a seq/coll and returns the sum of all elements in the
  seq/coll"
  [ls]
  (apply +' ls))

(defn product
  "Accepts a seq/coll and returns the product of all elements in the
  seq/coll"
  [ls]
  (apply *' ls))

(defn square [x] (* x x))
(defn cube [x] (* x x x))

(defn expt-big
  "Returns a to the m-th power, both must be zero or positive integers"
  [a m]
  (loop [i 0 res 1]
    (if (= i m)
      res
      (recur (inc i) (*' a res)))))

(defn sum-digits
  "Returns the sum of all digits in n"
  [n]
  (sum (map #(read-string (str %)) (str n))))

(defn expt
  [a m]
  (if (zero? m) 1 (* a (expt a (dec m)))))

(defn gcd
  "Accepts two numbers and returns the greatest common divisors of
  those numbers"
  [a b]
  (loop [i a j b]
    (if (= i j)
      i
      (if (> i j)
        (recur j (- i j))
        (recur i (- j i))))))

(defn prime?
  "Accepts a number and returns true if it is a prime, and false
  otherwise"
  [p]
  (cond (<= p 20) (if (some #(= % p) [2 3 5 7 11 13 17 19]) true false)
        (even? p) false
        :else (let [lim (inc (Math/sqrt p))]
                (loop [i 3]
                  (if (> i lim)
                    true
                    (if (zero? (rem p i))
                      false
                      (recur (+ 2 i))))))))

(defn rude-lcm
  [[a & xs] res]
  (if (empty? xs)
    (conj res a)
    (if (some #(zero? (rem % a)) xs)
      (-> #(if (zero? (rem % a)) (quot % a) %)
          (map xs)
          (rude-lcm (if (prime? a) (conj res a) res)))
      (rude-lcm xs (conj res a)))))

(defn lcm
  "Accepts a seq/coll of numbers and returns the least common multiple
  of all numbers in the seq/coll"
  [ls]
  (product (rude-lcm ls [])))

(defn factors
  "Accepts a number n and returns the factors of n"
  [n]
  (loop [i 1 res []]
    (if (>= (* i i) n)
      (if (= (* i i) n) (conj res i) res)
      (recur (inc i)
             (if (= 0 (rem n i))
               (if (= i (quot n i)) (conj res i) (conj res i (quot n i)))
               res)))))

(defn count-factors
  "Accepts a number n and returns the factors of n"
  [n]
  (loop [i 1 res 0]
    (if (>= (* i i) n)
      (if (= (* i i) n)
        (inc res)
        res)
      (recur (inc i)
             (if (= 0 (rem n i))
               (if (= i (quot n i))
                 (inc res)
                 (+ 2 res))
               res)))))

(defn count-divs
  "Return the number of divisors of n"
  [n]
  (if (= n 1)
    1
    (let [plim (inc (Math/sqrt n))]
      (if (even? n)
        (loop [i 2 lim (quot n 2) res 2]
          (if (or (> i plim) (>= i lim))
            (if (= 4 n)
              3
              res)
            (if (zero? (rem n i))
              (let [tmp (quot n i)]
                (if (= tmp i)
                  (inc res)
                  (recur (inc i)
                         tmp
                         (+ 2 res))))
              (recur (inc i)
                     lim
                     res))))
        (loop [i 3 lim (quot n 3) res 2]
          (if (or (> i plim) (>= i lim))
            res
            (if (zero? (rem n i))
              (let [tmp (quot n i)]
                (if (= tmp i)
                  (inc res)
                  (recur (+ 2 i)
                         tmp
                         (+ 2 res))))
              (recur (+ 2 i)
                     lim
                     res))))))))


(defn next-prime
  "Returns the smallest prime that is larger than x"
  [x]
  (cond (= 1 x) 2
        (= 2 x) 3
        (even? x) (next-prime (inc x))
        :else (loop [i (+ 2 x)]
                (if (prime? i) i (recur (+ 2 i))))))


;; (defn sum-primes
;;   "Returns the smallest prime that is larger than x"
;;   [lim]
;;   (+ 2 (loop [i (+ 2 x)]
;;          (if (prime? i) i (recur (+ 2 i))))))

(defn prime-list
  "Returns the n first positive prime numbers"
  [n]
  (loop [i 1 cur 2 res '()]
    (if (= i n)
      (cons cur res)
      (recur (inc i)
             (next-prime cur)
             (cons cur res)))))

(defn prime-list-vector
  "Returns the n first positive prime numbers"
  [n]
  (loop [i 1 cur 2 res []]
    (if (= i n)
      (conj res cur)
      (recur (inc i)
             (next-prime cur)
             (conj res cur)))))

(defn primes-under
  "Returns all positive primes less than n"
  [n]
  (loop [i 2 res []]
    (if (> i n)
      res
      (recur (next-prime i) (conj res i)))))

(defn suma-prima
  "Returns the sum of n first positive prime numbers"
  [n ctype]
  (if (= :list ctype)
    (reduce + (prime-list n))
    (reduce + (prime-list-vector n))))

(defn abs
  [n]
  (if (neg? n) (- n) n))

(defn now
  []
  (java.util.Date.))

(defn numcol
  "Returns a vector of digits of a number n"
  [n]
  (loop [i n res '()]
    (if (< i 10)
      (cons i res)
      (recur (quot i 10) (cons (rem i 10) res)))))


(defn colnum
  "Returns a number made from a vector of digits"
  [ls]
  (loop [[x & xs] ls res 0]
    (if (empty? xs)
      (+ (* 10 res) x)
      (recur xs (+ (* 10 res) x)))))

(defn palin?
  "Returns true if n is a palindromic number"
  [n]
  (let [col (numcol n)]
    (= col (reverse col))))

(defn palins [n]
  (let [gen-pal (fn gen-pal [n mcol]
                  (let [nn (inc n)
                        ncol (numcol nn)
                        stat (if (odd? (count mcol))
                               (not= (* 2 (count ncol))
                                     (inc (count mcol)))
                               (not= (* 2 (count ncol))
                                     (count mcol)))]
                    (if stat
                      (colnum (concat ncol
                                      (if (odd? (count mcol))
                                        (reverse (butlast (butlast ncol)))
                                        (reverse (butlast ncol)))))
                      (colnum (concat ncol
                                      (if (odd? (count mcol))
                                        (reverse (butlast ncol))
                                        (reverse ncol)))))))
        npal (fn [n]
               (let [mcol (numcol n)
                     lcol (vec (take (quot (count mcol) 2) mcol))
                     rlcol (reverse lcol)
                     rcol (take-last (quot (count mcol) 2) mcol)
                     midnum (first (drop (quot (count mcol) 2) mcol))
                     stat (odd? (count mcol))]
                 (if (<= (count mcol) 2)
                   (loop [i (inc n)]
                     (if (palin? i)
                       i
                       (recur (inc i))))
                   (if (palin? n)
                     (gen-pal (colnum (if stat
                                        (conj lcol midnum)
                                        lcol)) mcol)
                     (if (> (colnum rlcol)
                            (colnum rcol))
                       (colnum (concat lcol
                                       (if stat
                                         [midnum]
                                         [])
                                       rlcol))
                       (gen-pal (colnum (if stat
                                          (conj lcol midnum)
                                          lcol)) mcol))))))
        m (if (palin? n)
            n
            (npal n))]
    (iterate npal m)))

(defn palins-under
  "Returns all palindromic numbers less than n"
  [n]
  (take-while #(< % n) (palins 1)))


(defn pfactors
  [nt]
  (let [lim (Math/sqrt nt)]
    (loop [p 2 n nt res []]
      (let [d (quot n p) r (rem n p)]
        (if (> p lim)
          (sort (distinct (conj res p d)))
          (if (zero? r)
            (if (or (= 1 d) (prime? d))
              (sort (distinct (conj res p d)))
              (recur 2 d (conj res p)))
            (recur (next-prime p) n res)))))))

(defn totient
  [n]
  (let [pfacts (pfactors n)]
    (* n (product (map #(- 1 (/ 1 %)) pfacts)))))






