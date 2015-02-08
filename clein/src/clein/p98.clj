(ns clein.p98)

(set! *unchecked-math* false)

(defn ^boolean psqr?
  [p]
  (let [ck (Math/sqrt p)]
    (= (Math/floor ck) (Math/ceil ck))))

(defn ^long isqrt
  [^long n]
  (int (Math/ceil (Math/sqrt n))))

(defn pitas
  [^long n]
  (loop [i (int n) ires (int 0)]
    (if (>= i (* 2 n))
      ires
      (let [is (+ (* n n) (* i i))]
        (if (psqr? is)
          (recur (inc i)
                 (+ ires (quot i 2)))
          (recur (inc i)
                 ires))))))

(defn ^long sqr [^long x] (* x x))

(defn ^longs freaks
  [^long n]
  (for [i (range 1 n)
        j (range i n)
        k (range j n)
        :let [isq (* i i)
              jsq (* j j)
              ksq (* k k)
              msq (min (+ isq (sqr (+ j k)))
                       (+ jsq (sqr (+ i k)))
                       (+ ksq (sqr (+ i j))))]
        :when (psqr? msq)]
    1))

(defn ^long euler86
  [^long lim]
  (loop [i (int 1) res (int 0)]
    (if (>= res lim)
      i
      (let [jres
            (loop [j (int 1) jrest (int 0)]
              (if (>= (+ res jrest) lim)
                i
                (if (>= j i)
                  jrest
                  (let [kres
                        (loop [k (int 1) krest (int 0)]
                          (if (>= k j)
                            krest
                            (let [isq (* i i)
                                  jsq (* j j)
                                  ksq (* k k)
                                  msq (min (+ isq (sqr (+ j k)))
                                           (+ jsq (sqr (+ i k)))
                                           (+ ksq (sqr (+ i j))))]
                              (if (psqr? msq)
                                (recur (inc k)
                                       (inc krest))
                                (recur (inc k)
                                       krest)))))]
                    (recur (inc j) (+ jrest kres))))))]
        (recur (inc i) (+ res jres))))))


(defn euler94
  [^long lim]
  (let [llim (quot lim 3)]
    (loop [i (int 5) res []]
      (if (> i llim)
        res
        (let [a (quot (dec i) 2)
              b (quot (inc i) 2)]
          (recur (+ i 2)
                 (if (psqr? (- (sqr i) (sqr a)))
                   (if (psqr? (- (sqr i) (sqr b)))
                     (conj res
                           [i i (* 2 a)]
                           [i i (* 2 b)])
                     (conj res [i i (* 2 a)]))
                   (if (psqr? (- (* i i) (* b b)))
                     (conj res [i i (* 2 b)])
                     res))))))))

(defn ^long area-triangles
  [^long lim]
  (loop [i (int 2) res 0]
    (if (>= (* 3 i) lim)
      res
      (let [a (inc i) b (dec i)
            p1 (quot (+ i i a) 2)
            p2 (quot (+ i i b) 2)
            asq (*' p1 (- p1 i) (- p1 i) (- p1 a))
            bsq (*' p2 (- p2 i) (- p2 i) (- p2 b))]
        (if (psqr? asq)
          (if (psqr? bsq)
            (recur (inc i) (+ res
                              (+ (* 2 i) a)
                              (+ (* 2 i) b)))
            (recur (inc i) (+ res (+ (* 2 i) a))))
          (if (psqr? bsq)
            (recur (inc i) (+ res (+ (* 2 i) b)))
            (recur (inc i) res)))))))

(defn ^long sol94
  [^long lim]
  (area-triangles lim))


