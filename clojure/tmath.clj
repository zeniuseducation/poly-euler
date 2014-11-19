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

;; Problem 3 elapsed time 5-6 msecs


