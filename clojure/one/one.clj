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
  (let [refs (boolean-array (+ 1 n))]
    (loop [i (int 1) res (int 0)]
      (if (aget refs i)
        res
        (let [rems (int (rem (* 10 i) n))]
          (if (== 0 rems)
            0
            (do (aset refs i true)
                (recur rems (+ 1 res)))))))))

(defn ^long max-cycle
  [^long n]
  (loop [i (int n) p (int n) res (int 0)]
    (if (> res i)
      [p res]
      (let [tmp (int (find-cycle i))]
        (if (> tmp res)
          (recur (- i 1) i tmp)
          (recur (- i 1) p res))))))


