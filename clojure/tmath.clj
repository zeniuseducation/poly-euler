(ns poly-euler.tmath)

(set! *unchecked-math* true)

(defn ^boolean prime?
  "Efficient prime, but cannot check for even numbers"
  [^long p]
  (let [lim (+ 1 (int (Math/sqrt p)))]
    (loop [i (long 3)]
      (if (> i lim)
        true
        (if (== 0 (rem p i))
          false
          (recur (+ i 2)))))))

(defn ^long sum-primes
  "Efficient summation of primes"
  [^long lim]
  (loop [i (long 7) res (long 10)]
    (if (> i lim)
      res
      (recur (+ i 2)
             (if (prime? i) (+ i res) res)))))

;; Problem 10 fastest 1,304 msecs

(defn expt
  [a m]
  (if (zero? m) 1 (*' a (expt a (dec m)))))

(def limits (expt 10 999))

(defn fibolim
  [lim]
  (loop [i (bigint 1) j (bigint 1) idx (int 1)]
    (if (> i lim)
      idx
      (recur (+ i j) i (+ 1 idx)))))

;; Problem 25 fastest 5-7msecs

(defn ^long next-prime
  [^long p]
  (cond
   (== p 2) 3
   :else (loop [i (+ 2 (long p))]
           (if (prime? i)
             i
             (recur (+ i 2))))))

(defn ^boolean true-prime?
  [^long p]
  (cond (== 2 p) true
        (== 0 (rem p 2)) false
        :else (let [lim (+ 1 (int (Math/sqrt p)))]
                (loop [i (long 3)]
                  (if (> i lim)
                    true
                    (if (== 0 (rem p i))
                      false
                      (recur (+ i 2))))))))

(defn ^longs pfactors
  "Prime factorisation using transient"
  [^long n]
  (loop [i (long 2) p (long n) lasti (long 2) res (transient [])]
    (if (true-prime? p)
      (if (== p lasti)
        (persistent! res)
        (persistent! (conj! res p)))
      (let [rems (rem p i) divs (quot p i)]
        (if (== 0 rems)
          (recur 2 divs i (conj! res i))
          (recur (next-prime i) p lasti res))))))

(defn ^longs nth-prime
  "Returns the first i-th positive primes"
  [^long i]
  (loop [p (long 2) idx (int 1)]
    (if (== idx i)
      p
      (recur (next-prime p) (+ 1 idx)))))

(defn ^long count-factors
  [^long n]
  (let [lim (int (inc (Math/sqrt n)))]
    (if (even? n)
      (loop [i (int 2) res (int 2)]
        (if (> i lim)
          res
          (let [divs (quot n i)]
            (if (== 0 (rem n i))
              (if (== i divs)
                (inc res)
                (recur (inc i) (+ 2 res)))
              (recur (inc i) res)))))
      (loop [i (int 3) res (int 2)]
        (if (> i lim)
          res
          (let [divs (quot n i)]
            (if (== 0 (rem n i))
              (if (== i divs)
                (inc res)
                (recur (+ 2 i) (+ 2 res)))
              (recur (+ 2 i) res))))))))

;; Problem 3 elapsed time 5-6 msecs

(defn ^longs first-triangle-having-lim-factors
  [^long i ^long lim]
  (loop [n (long i)]
    (let [triangle (long (quot (* n (inc n)) 2))
          factors (long (count-factors triangle))]
      (if (>= factors lim)
        [n triangle]
        (recur (inc n))))))


