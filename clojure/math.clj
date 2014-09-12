(defn prime?
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

(defn factors
  [n]
  (let [lim (inc (Math/sqrt n))]
    (loop [i 2 res []]
      (if (> i lim)
        res
        (recur (inc i)
               (if (= 0 (rem n i))
                 (conj res i (quot n i))
                 res))))))

(defn div?
  "Returns true if a is evenly divisible by m"
  [a m]
  (zero? (rem a m)))

;; PROBLEM NO 6

(defn sum
  [ls]
  (apply +' ls))

(defn product
  [ls]
  (apply *' ls))

(defn square [x] (* x x))


(defn next-prime
  "Returns the next prime larger than x, assuming x is prime"
  [x]
  (cond (= 2 x) 3
        (even? x) (next-prime (inc x))
        :else (loop [i (+ 2 x)]
                (if (prime? i) i (recur (+ 2 i))))))

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
  [ls]
  (product (rude-lcm ls [])))

(defn gcd
  [a b]
  (if (= a b)
    a
    (if (> a b)
      (gcd b (- a b))
      (gcd a (- b a)))))
