(ns eulerpeter.core
  (:gen-class))

(defn sum [x] (reduce + x))

(defn product [x] (reduce * x))

(defn faktor
  [n]
  (loop [i 1 res []]
    (if (>= (* i i) n)
      (if (= (* i i) n) (conj res i) res)
      (recur (inc i)
             (if (= 0 (rem n i))
               (if (= i (quot n i)) (conj res i) (conj res i (quot n i)))
               res)))))

(defn num2seq
  ;;example 123 -> (1 2 3)
  [x]
  (mapv #(- (int %) 48) (seq (str x))))

(defn seq2num
  ;;exam (1 2 3) -> 123
  [coll]
  (Integer/parseInt (apply str coll)))

(defn prime?
  [x]
  (if (or
        (= 2 x)
        (= 3 x))
    true
    (if (or
          (= 1 x)
          (zero? (rem x 2))
          (zero? (rem x 3)))
      false
      (loop [i 3]
        (if (> (* i i) x)
          true
          (if (zero? (rem x i))
            false
            (recur (+ i 2))))))))

(defn next-prime
  [x]
  (if (>= 1 x)
    2
    (loop [i (if (even? x) (inc x) (+ x 2))]
      (if (prime? i)
        i
        (recur (+ i 2))))))

(defn prev-prime
  [x]
  (cond (<= x 2) nil
        (= x 3) 2
        :else (loop [i (if (even? x) (dec x) (- x 2))]
                (if (or (prime? i) (>= 1 i))
                  i
                  (recur (- i 2))))))

(defn prime-bellow
  [lim]
  (loop [i (prev-prime lim) res []]
    (if (nil? i)
      res
      (recur (prev-prime i) (conj res i)))))

(defn c-prime-bellow
  [lim]
  (loop [i (prev-prime lim) res 0]
    (if (nil? i)
      res
      (recur (prev-prime i) (inc res)))))

(defn prime-th
  [th]
  (loop [lim 0 res 1]
    (if (>= lim th)
      res
      (recur (inc lim) (next-prime res)))))

(defn sum-prime-bellow
  ;; n=5 -> + 2 3
  [lim]
  (loop [i 3 res 2]
    (if (<= lim i)
      res
      (recur (next-prime i) (+ res i)))))

(defn sum-prime-until
  ;; n=5 -> + 2 3
  [lim]
  (loop [i 2 c [] res 0]
    (if (< lim (+ res i))
      (vec [res c])
      (recur (next-prime i) (conj c i) (+ res i)))))

(defn cons-prime-sum?
  [n]
  (= n (first (sum-prime-until n))))

(defn pow
  ;; a^b
  [a b]
  (apply *' (repeat b a)))

(defn pascal-tri
  ;;generate pascal line n
  [n]
  (loop [line n res '(1)]
    (if (= 1 line)
      res
      (recur
        (dec line)
        (concat '(1) (map +' res (rest res)) '(1))))))

(defn pascal-trape [coll]
  (iterate #(map +' (concat % [0]) (concat [0] %)) coll))

(defn pt [coll]
  (#(map +' (concat [] [0] %) (concat [] % [0])) coll))


(defn gcd
  ;;FPB
  [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn lcm
  ;;KPK
  [a b]
  (/ (* a b) (gcd a b)))

(defn flatten- [coll] (let [[x & xs] coll]
                        (concat (if (coll? x)
                                  (flatten- x)
                                  [x])
                                (if (coll? xs)
                                  (flatten- xs)))))

(defn flat [coll] (mapcat #(if (coll %)
                            (flat %)
                            [%]) coll))

(defn kpk [coll] (letfn [(fpb
                           [a b]
                           (if (zero? b)
                             a
                             (fpb b (mod a b))))]
                   (reduce #(/ (* %1 %2) (fpb %1 %2)) coll)))

(defn tot [x] (for [i (range 1 x) :when (= 1 (gcd i x))] i))

(fn tot [x] (letfn [(gcd
                      ;;FPB
                      [a b]
                      (if (zero? b)
                        a
                        (gcd b (mod a b))))]
              (if (= 1 x) 1 (count (for [i (range 1 x) :when (= 1 (gcd i x))] i)))))

(defn bp? [x] (if (prime? x)
                (= x (/ (+ (next-prime x) (prev-prime x)) 2))
                false))

(defn bigdif [n c1 c2] (if (or (> n c1)
                               (> n c2))
                         (loop [i 1 res 0]
                           (if (>= i n)
                             res
                             (recur (+ 2 i) (if (or (and (= 0 (rem i c1)) (not (= 0 (quot i c1))))
                                                    (and (= 0 (rem i c2)) (not (= 0 (quot i c2)))))
                                              (+ res i)
                                              res))))
                         0))


;;

(defn tot [x] (letfn [(gcd
                        ;;FPB
                        [a b]
                        (if (zero? b)
                          a
                          (gcd b (mod a b))))]
                (if (= 1 x) 1 (count (for [i (range 1 x) :when (= 1 (gcd i x))] i)))))
