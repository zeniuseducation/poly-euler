(ns poly-euler.one)

(set! *unchecked-math* true)

(defn expn
  [a m]
  (if (= 1 m)
    a
    (if (= 0 (rem m 2))
      (expn (*' a a) (quot m 2))
      (*' a (expn (* a a) (quot (- m 1) 2))))))

(defn ^long find-cycle
  [^long n]
  (let [refs (boolean-array (+ 1 n))
        refs2 (boolean-array (+ 1 n))]
    (loop [i (int 1) res (int 0) res2 (int 0)]
      (if (aget refs2 i)
        res2
        (let [rems (int (rem (* 10 i) n))]
          (if (== 0 rems)
            0
            (if (aget refs i)
              (do (aset refs2 i true)
                  (recur rems res (+ 1 res2)))
              (do (aset refs i true)
                  (recur rems (+ 1 res) res2)))))))))

(defn ^long max-cycle
  [^long n]
  (loop [i (int n) p (int n) res (int 0)]
    (if (> res i)
      [p res]
      (let [tmp (int (find-cycle i))]
        (if (> tmp res)
          (recur (- i 1) i tmp)
          (recur (- i 1) p res))))))

(defn third [ls] (nth ls 2))

(defn ^longs euler28
  [^long lim]
  (->> (iterate #(let [[a b c] %
                       one (+ a (+ 8 (- a b)))
                       res (+ (* 4 one) c)]
                   [one a res])
                [6 1 25])
       (drop (dec (quot lim 2)))
       first third))

(defn ^long euler28a
  [^long lim]
  (->> (range (int 3) (inc (int lim)) (int 2))
       (pmap #(* 2 (+ (* % %) (- (* % %) (* 3 (- % 1))))))
       (reduce +) (+ 1)))

(defn ^long euler28b
  [^long lim]
  (loop [a (int 6) b (int 1) res (int 25) i (int 3)]
    (if (== i lim)
      res
      (let [tempa (+ a (+ 8 (- a b)))]
        (recur tempa a (+ res (* 4 tempa)) (+ 2 i))))))

(defn ^boolean prime?
  [^long p]
  (cond (< p 2) false
        (== 2 p) true
        (== 0 (rem p 2)) false
        :else (let [lim (+ 1 (int (Math/sqrt p)))]
                (loop [i (long 3)]
                  (if (> i lim)
                    true
                    (if (== 0 (rem p i))
                      false
                      (recur (+ i 2))))))))

(defn ^long next-prime
  [^long p]
  (cond (== p 2) 3
        (prime? (+ p 2)) (+ p 2)
        :else (next-prime (+ p 2))))

(defn ^long prev-prime
  [^long p]
  (cond (<= p 2) nil
        (== p 3) 2
        (prime? (- p 2)) (- p 2)
        :else (prev-prime (- p 2))))

(defn ^long sieves
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        refs (boolean-array (inc lim))]
    (loop [i (int 3) res (transient [2])]
      (if (>= i lim)
        (persistent! res)
        (recur (if (and (<= i llim) (not (aget refs i)))
                 (loop [p (int (* i i))]
                   (if (<= p (- lim 1))
                     (recur (do (aset refs p true)
                                (+ p (* 2 i))))
                     (+ 2 i)))
                 (+ 2 i))
               (if (aget refs i) res (conj! res i)))))))

(defn ^long euler27a
  [^long lim]
  (->> (for [b (sieves lim)]
         (loop [a (filter #(> (+ 1 % b) 0) (range (- lim) (inc lim)))
                cura (int (- lim)) res (int 1)]
           (if (empty? a)
             [res cura b]
             (let [resn (int (loop [n (int 1) res (int 1)]
                               (if (prime? (+ (* n n)
                                              (* (first a) n)
                                              b))
                                 (recur (+ 1 n) (+ 1 res))
                                 res)))]
               (if (> resn res)
                 (recur (rest a) (first a) resn)
                 (recur (rest a) cura res))))))
       (sort-by first) last rest (apply *)))

(defn ^long euler27
  [^long lim]
  (loop [b 2 [rr ar br & _] [0,0,0]]
    (if (> b lim)
      (* ar br)
      (let [[rar aar bar & _]
            (loop [a (int (- lim)) cura (int (- lim)) resa (int 1)]
              (cond
               (> a lim) [resa cura b]
               (<= (+ a b 1) 0) (recur (+ 1 a) cura resa)
               :else (let [resn (int (loop [n (int 1) res (int 1)]
                                       (if (prime? (+ (* n n)
                                                      (* a n)
                                                      b))
                                         (recur (+ 1 n) (+ 1 res))
                                         res)))]
                       (if (> resn resa)
                         (recur (+ 1 a) a resn)
                         (recur (+ 1 a) cura resa)))))]
        (if (> rar rr)
          (recur (next-prime b) [rar aar bar])
          (recur (next-prime b) [rr ar br]))))))

(defn ^long euler27b
  [^long lim]
  (loop [b 997 [rr ar br & _] [0,0,0]]
    (if (< b rr)
      (* ar br)
      (let [[rar aar bar & _]
            (loop [a (int (- lim 1)) cura (int (- lim)) resa (int 1)]
              (cond
               (< a (- lim)) [resa cura b]
               (<= (+ a b 1) 0) (recur (- a 2) cura resa)
               :else (let [resn (int (loop [n (int 1) res (int 1)]
                                       (if (prime? (+ (* n n)
                                                      (* a n)
                                                      b))
                                         (recur (+ 1 n) (+ 1 res))
                                         res)))]
                       (if (> resn resa)
                         (recur (- a 2) a resn)
                         (recur (- a 2) cura resa)))))]
        (if (> rar rr)
          (recur (prev-prime b) [rar aar bar])
          (recur (prev-prime b) [rr ar br]))))))

(defn ^long pow
  [^long a ^long m]
  (loop [a (int a) m (int m) res (int 1)]
    (if (== m 0) res (recur a (- m 1) (* a res)))))

(defn ^long sumfif
  [^long n]
  (loop [i (int n) res (int 0)]
    (if (< i 10)
      (+ res (pow i 5))
      (recur (quot i 10) (+ res (pow (rem i 10) 5))))))

(defn ^long euler30
  []
  (loop [i (int (* 6 (pow 9 5))) res (int 0)]
    (if (== i 10)
      res
      (let [sum5 (int (sumfif i))]
        (if (== sum5 i)
          (recur (- i 1) (+ res i))
          (recur (- i 1) res))))))

(defn ^long euler30p
  [^long lim]
  (letfn [(sum-fifth?
            [^long n]
            (loop [i (int n) res (int 0)]
              (if (> res n)
                false
                (if (< i 10)
                  (== n (+ res (pow i 5)))
                  (recur (quot i 10) (+ res (pow (rem i 10) 5)))))))
          (sumup [^long n]
            (let [lim (int (* 1000 (+ n 1)))]
              (loop [i (int (* 1000 n)) res (int 0)]
                (if (== i lim)
                  res
                  (if (sum-fifth? i)
                    (recur (+ 1 i) (+ res i))
                    (recur (+ 1 i) res))))))]
    (reduce + (pmap sumup (range lim)))))

(defn ^long euler30r
  [^long lim]
  (letfn [(sum-fifth?
            [^long n]
            (loop [j (int n) res (int 0)]
              (if (> res n)
                false
                (if (< j 10)
                  (== n (+ res (pow j 5)))
                  (recur (quot j 10) (+ res (pow (rem j 10) 5)))))))]
    (loop [i (int lim) res (int 0)]
      (if (== i 10)
        res
        (if (sum-fifth? i)
          (recur (- i 1) (+ res i))
          (recur (- i 1) res))))))










