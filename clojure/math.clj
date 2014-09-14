(defn div?
  "Returns true if a is evenly divisible by m"
  [a m]
  (zero? (rem a m)))

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
  (if (= a b)
    a
    (if (> a b)
      (gcd b (- a b))
      (gcd a (- b a)))))

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
  (let [lim (inc (Math/sqrt n))]
    (loop [i 2 res []]
      (if (> i lim)
        res
        (recur (inc i)
               (if (= 0 (rem n i))
                 (if (= i (quot n i))
                   (conj res i)
                   (conj res i (quot n i)))
                 res))))))

(defn next-prime
  "Returns the smallest prime that is larger than x"
  [x]
  (cond (= 1 x) 2
        (= 2 x) 3
        (even? x) (next-prime (inc x))
        :else (loop [i (+ 2 x)]
                (if (prime? i) i (recur (+ 2 i))))))

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

(defn suma-prima
  "Returns the sum of n first positive prime numbers"
  [n ctype]
  (if (= :list ctype)
    (reduce + (prime-list n))
    (reduce + (prime-list-vector n))))









