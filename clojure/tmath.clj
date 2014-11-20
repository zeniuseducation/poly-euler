(ns poly-euler.tmath)

(set! *unchecked-math* true)

(defn ^boolean prime?
  "Efficient prime, but cannot check for even numbers"
  [^long p]
  (loop [i (long 3)]
    (if (> (* i i) p)
      true
      (if (== 0 (rem p i))
        false
        (recur (+ i 2))))))

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

(defn ^long fibolim
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

(defn ^long sum-pdivisors
  [^long n]
  (let [lim (Math/sqrt n)]
    (if (even? n)
      (loop [i (int 2) res 1]
        (if (> i lim)
          res
          (let [divs (quot n i)]
            (if (== 0 (rem n i))
              (if (== i divs)
                (+ i res)
                (recur (inc i) (+ res i divs)))
              (recur (inc i) res)))))
      (loop [i (int 3) res 1]
        (if (> i lim)
          res
          (let [divs (quot n i)]
            (if (== 0 (rem n i))
              (if (== i divs)
                (+ res i)
                (recur (+ 2 i) (+ i res divs)))
              (recur (+ 2 i) res))))))))

(defn ^long sum-amic
  [^long lim]
  (loop [i (long 2) res (long 0)]
    (if (>= i lim)
      res
      (let [amic (long (sum-pdivisors i))]
        (if (== amic i)
          (recur (inc i) res)
          (let [div-amic (long (sum-pdivisors amic))]
            (if (== i div-amic)
              (recur (inc i) (+ i res))
              (recur (inc i) res))))))))

;; Problem 3 elapsed time 5-6 msecs

(defn ^longs first-triangle-having-lim-factors
  [^long i ^long lim]
  (loop [n (long i)]
    (let [triangle (long (quot (* n (inc n)) 2))
          factors (long (count-factors triangle))]
      (if (>= factors lim)
        [n triangle]
        (recur (inc n))))))

(defn ^long collatz
  [^long i]
  (loop [n (long i) res (long 1)]
    (if (== n 1)
      res
      (if (even? n)
        (recur (quot n 2) (inc res))
        (recur (inc (* 3 n)) (inc res))))))

(def rcollatz
  (memoize
   (fn ^long rcollatz [^long i]
     (if (== 1 i)
       1
       (if (even? i)
         (inc (rcollatz (quot i 2)))
         (inc (rcollatz (inc (* 3 i)))))))))

(defn ^long max-collatz-under-lim
  [^long lim]
  (loop [i (long 500001) res (long i) lres (int 1)]
    (if (> i lim)
      res
      (let [colls (collatz i)]
        (if (> colls lres)
          (recur (+ 2 i) i colls)
          (recur (+ 2 i) res lres))))))

(defn ^long suma-prima-parts
  [^long n]
  (->> (range (+ 1 (* 1000 n))
              (* 1000 (inc n))
              2)
       (filter prime?)
       (reduce +)))

(defn ^long partial-sum
  [^long n]
  (let [lim (* (inc n) 1000)]
    (loop [i (long (+ 1 (* 1000 n))) res 0]
      (if (> i lim)
        res
        (recur (+ 2 i)
               (if (prime? i)
                 (+ i res)
                 res))))))

(defn ^long suma-prima
  [^long lim]
  (->> (range 1 lim)
       (pmap partial-sum)
       (reduce +)
       (+ (+ 2 (reduce + (filter prime? (range 3 1000 2)))))))

(defn ^longs primes-to
  "Returns a list of all primes from 2 to n"
  [^long n]
  (let [root (-> n Math/sqrt int)]
    (loop [i (int 2), a (boolean-array (inc n)), result (transient [])]
      (if (> i n)
        (persistent! result)
        (recur (inc i)
               (if (and (<= i root) (not (aget a i)))
                 (loop [arr a, j (* i i)]
                   (if (> j n)
                     arr
                     (recur (do (aset arr j true) arr)
                            (+ j i))))
                 a)
               (if (not (aget a i))
                 (conj! result i)
                 result))))))

(defn ^long sum-sieves
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        refs (boolean-array (inc lim))]
    (loop [i (int 3) res (long 0)]
      (if (>= i lim)
        (+ 2 res)
        (recur (if (and (<= i llim) (not (aget refs i)))
                 (loop [p (int (* i i))]
                   (if (<= p (- lim 1))
                     (recur (do (aset refs p true)
                                (+ p (* 2 i))))
                     (+ 2 i)))
                 (+ 2 i))
               (if (aget refs i) res (+ res i)))))))

(defn ^long sum-sieves2
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        refs (boolean-array (unchecked-inc lim))]
    (loop [i (long 3) res (transient [])]
      (if (> i llim)
        (+ 2
           (reduce unchecked-add (persistent! res))
           (reduce unchecked-add (filter #(not (aget refs %))
                                         (range i lim 2))))
        (recur (do (loop [p (int (unchecked-multiply i i))]
                     (if (> p (unchecked-dec lim))
                       nil
                       (recur (do (aset refs p true)
                                  (unchecked-add p (unchecked-multiply 2 i))))))
                   (loop [n (int (unchecked-add 2 i))]
                     (if (not (aget refs n))
                       n
                       (recur (unchecked-add 2 n)))))
               (conj! res i))))))
