(ns euler.prob301-400)

(load-file "math.clj")

(def pms [2,4,8])
(def limit 10000000000)

(defn pmprime
  [n]
  (inc (expt 2 n)))

(defn expt [a m] (if (zero? m) 1 (*' a (expt a (dec m)))))
(defn sqr [x] (* x x))

(defn base-primes
  [lim]
  (for [i (range 1 lim)
        :when (prime? (inc (expt 2 i)))]
    (inc (expt 2 i))))

(defn oneprime
  [lim pm]
  (let [p (pmprime pm)]
    (for [i (range 30)
          q (range 20)
          :let [pow2 (+' pm (*' 2 i) (if (zero? i) 0 -1))
                powp (dec (*' 2 q))]
          :when (and (zero? (rem pow2 3))
                     (zero? (rem powp 3))
                     (< (*' (expt p q) (expt 2 i)) lim))]
      (*' (expt p q) (expt 2 i)))))

(defn twoprimes
  [lim pm1 pm2]
  (let [p1 (pmprime pm1)
        p2 (pmprime pm2)]
    (for [i (range 30)
          q (range 10)
          r (range 10)
          :let [pow2 (+' pm1 pm2 (*' 2 i) (if (zero? i) 0 -1))
                powp1 (dec (*' 2 q))
                powp2 (dec (*' 2 r))]
          :when (and (zero? (rem pow2 3))
                     (zero? (rem powp1 3))
                     (zero? (rem powp2 3))
                     (< (*' (expt p1 q) (expt p2 r)(expt 2 i)) lim))]
      (*' (expt p1 q) (expt p2 r) (expt 2 i)))))

(defn threeprimes
  [lim pm1 pm2 pm3]
  (let [p1 (pmprime pm1)
        p2 (pmprime pm2)
        p3 (pmprime pm3)]
    (for [i (range 30)
          q (range 10)
          r (range 10)
          s (range 5)
          :let [pow2 (+' pm1 pm2 pm3 (*' 2 i) (if (zero? i) 0 -1))
                powp1 (dec (*' 2 q))
                powp2 (dec (*' 2 r))
                powp3 (dec (*' 2 s))]
          :when (and (zero? (rem pow2 3))
                     (zero? (rem powp1 3))
                     (zero? (rem powp2 3))
                     (zero? (rem powp3 3))
                     (< (*' (expt p3 s) (expt p1 q)(expt p2 r)(expt 2 i)) lim))]
      (*' (expt p1 q) (expt p2 r) (expt p3 s) (expt 2 i)))))

(defn allprimes
  [lim pm1 pm2 pm3 pm4]
  (let [p1 (pmprime pm1)
        p2 (pmprime pm2)
        p3 (pmprime pm3)
        p4 (pmprime pm4)]
    (for [i (range 30)
          q (range 10)
          r (range 10)
          s (range 5)
          t (range 5)
          :let [pow2 (+' pm1 pm2 pm3 pm4 (*' 2 i) (if (zero? i) 0 -1))
                powp1 (dec (*' 2 q))
                powp2 (dec (*' 2 r))
                powp3 (dec (*' 2 s))
                powp4 (dec (*' 2 t))]
          :when (and (zero? (rem pow2 3))
                     (zero? (rem powp1 3))
                     (zero? (rem powp2 3))
                     (zero? (rem powp3 3))
                     (zero? (rem powp4 3))
                     (< (*' (expt p4 t) (expt p3 s)(expt p1 q)(expt p2 r)(expt 2 i)) lim))]
      (*' (expt p1 q) (expt p2 r) (expt p3 s) (expt p3 t) (expt 2 i)))))

(defn sol342
  [lim]
  (time (sum (distinct
              (concat (oneprime lim 2)
                      (oneprime lim 1)
                      (oneprime lim 4)
                      (oneprime lim 8)
                      (twoprimes lim 1 2)
                      (twoprimes lim 2 4)
                      (twoprimes lim 1 4)
                      (twoprimes lim 1 8)
                      (twoprimes lim 2 8)
                      (twoprimes lim 4 8)
                      (threeprimes lim 1 2 4)
                      (threeprimes lim 1 4 8)
                      (threeprimes lim 1 2 8)
                      (threeprimes lim 2 4 8)
                      (allprimes lim 1 2 4 8))))))

(defn otherprimes
  [lim]
  (for [i (range 0 lim 3)
        :when (prime? (inc (expt 2 i)))]
    [i (inc (expt 2 i))]))

