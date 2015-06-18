(ns alfa.common
  (:require
    [clojure.set :refer [union]]
    [clojure.core.reducers :as r]))

(defmacro defm [nama binding body]
  `(def ~nama (memoize (fn ~binding ~body))))

(defn psqr?
  [n]
  (let [tmp (Math/sqrt n)]
    (== tmp (long tmp))))

(defn triangle
  [^long n]
  (quot (* n (+ 1 n)) 2))

(defn square
  [n]
  (* n n))

(defn ^longs divisors
  [^long n]
  (if (even? n)
    (loop [i (int 2) res [1 n]]
      (if (>= (* i i) n)
        (if (== (* i i) n)
          (vec (sort (conj res i)))
          (vec (sort res)))
        (recur (+ i 1)
               (if (== 0 (rem n i))
                 (conj res i (quot n i))
                 res))))
    (loop [i (int 3) res [1 n]]
      (if (>= (* i i) n)
        (if (== (* i i) n)
          (vec (sort (conj res i)))
          (vec (sort res)))
        (recur (+ i 2)
               (if (== 0 (rem n i))
                 (conj res i (quot n i))
                 res))))))

(defn ^boolean prime?
  [^long n]
  (cond
    (== n 2) true
    (even? n) false
    :else (let [llim (int (Math/sqrt n))]
            (loop [i (int 3)]
              (if (> i llim)
                true
                (if (== 0 (rem n i))
                  false
                  (recur (+ i 2))))))))

(defn ^long next-prime
  [^long n]
  (if (even? n)
    (if (prime? (+ n 1))
      (+ n 1)
      (next-prime (+ n 1)))
    (if (prime? (+ n 2))
      (+ n 2)
      (next-prime (+ n 2)))))


(defn subseqs
  "Generating all possible subsequences of xs"
  [xs]
  (let [k (count xs)]
    (loop [i (int 0) res [] tres [[]]]
      (if (== i k)
        (concat res tres)
        (recur (+ 1 i)
               (concat res tres)
               (for [x xs r tres
                     :when (not-any? #{x} r)]
                 (conj r x)))))))

(defn subsets
  "Generating all possible subsets of xs"
  [xs]
  (let [k (count xs)]
    (loop [i (int 0) res #{} tres #{#{}}]
      (if (== i k)
        (union res tres)
        (recur (+ 1 i)
               (union res tres)
               (set (for [x xs r tres
                          :when (not-any? #{x} r)]
                      (conj r x))))))))

(defn combine
  "Take k elements from xs"
  [xs k]
  (loop [i (int 0) res #{#{}}]
    (if (== i k)
      res
      (recur (+ 1 i)
             (set (for [x xs r res
                        :when (not-any? #{x} r)]
                    (conj r x)))))))

(defn combinations
  "Generate the combinations of n elements from a list of [0..m)"
  [m n]
  (let [xs (range m)]
    (loop [i (int 0) res #{#{}}]
      (if (== i n)
        res
        (recur (+ 1 i)
               (set (for [x xs r res
                          :when (not-any? #{x} r)]
                      (conj r x))))))))

(defn min-by
  [f col]
  (loop [[x & xs] col res (f x) resi x]
    (if x
      (let [tmp (f x)]
        (if (< tmp res)
          (recur xs tmp x)
          (recur xs res resi)))
      resi)))

(defn max-by
  [f col]
  (loop [[x & xs] col res (f x) resi x]
    (if x
      (let [tmp (f x)]
        (if (> tmp res)
          (recur xs tmp x)
          (recur xs res resi)))
      resi)))

(defn fact
  [n]
  (apply *' (range 1 (+ n 1))))

(defn modex
  [a b m]
  (cond (= b 0) 1
        (= b 1) a
        :else (let [x (modex a (quot b 2) m)]
                (rem (*' x x (if (even? b) 1 a)) m))))

(defn numcol
  [n]
  (loop [i n res []]
    (if (< i 10)
      (cons i res)
      (recur (quot i 10)
             (cons (rem i 10) res)))))

(defn expt
  [^long a ^long m]
  (cond (== m 0) 1
        (== m 1) a
        :else (let [res (expt a (quot m 2))]
                (*' res res (if (even? m) 1 a)))))

(defn fexpt
  [a m]
  (cond (== m 0) 1
        (== m 1) a
        :else (let [res (fexpt a (quot m 2))]
                (*' res res (if (even? m) 1 a)))))
(defn colnum
  [^longs ls]
  (loop [[x & xs] ls res 0]
    (if x
      (recur xs (+ (* 10 res) x))
      res)))

(defn permutes
  "Generating k permutations of xs"
  [xs k]
  (loop [i (int 0) res [[]]]
    (if (== i k)
      res
      (recur (+ 1 i)
             (for [x xs r res]
               (conj r x))))))

(defn ^longs sieve
  [^long lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))]
    (loop [i (int 3) res (transient [2])]
      (if (> i lim)
        (persistent! res)
        (if (aget refs i)
          (do (when (<= i llim)
                (loop [j (int (* i i))]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j (* 2 i)))))))
              (recur (+ i 2) (conj! res i)))
          (recur (+ i 2) res))))))

(defn remove-one
  [a col]
  (loop [[x & xs] col res []]
    (if x
      (if (== x a)
        (vec (concat res xs))
        (recur xs (conj res x)))
      res)))

(defn permute
  "Generating k permutations of xs"
  [xs k]
  (let [refs (into {} (map #(vector %2 %) xs (range)))
        txs (range 0 (count xs))]
    (loop [i (int 0) res [[]]]
      (if (== i k)
        (map #(map (fn [x] (get refs x)) %) res)
        (recur (+ 1 i)
               (for [x txs r res
                     :when (not-any? #{x} r)]
                 (conj r x)))))))

(defn gcd [a b]
  (if (== 0 (rem a b))
    b
    (loop [i (int a) j (int b)]
      (cond (== 0 i) j
            (== 0 j) i
            (or (== 1 i) (== 1 j)) 1
            (< i j) (recur j (- j i))
            :else (recur j (- i j))))))

(defn count-digits
  [a]
  (count (str a)))


(defn psqr?
  [^long n]
  (let [tmp (Math/sqrt n)]
    (== tmp (long tmp))))

(defn modmul
  [a b m]
  (mod (*' a b) m))

(defn modadd
  [a b m]
  (mod (+' a b) m))

(defn isqrt
  [n]
  (long (Math/sqrt n)))

(defn sqrt
  [n]
  (Math/sqrt n))

(defn logbase
  [a b]
  (/ (Math/log b)
     (Math/log a)))

(defn pascal
  [^long lim]
  (take lim (iterate #(vec (cons 1 (map +' % (conj (vec (rest %)) 0)))) [1 1])))

(defn ^longs radical
  [^long n]
  (let [start (->> (iterate #(/ % 2) n)
                   (take-while integer?)
                   last)]
    (loop [i (int start) p (int 3) res (if (even? n) 2 1)]
      (if (== 1 i)
        res
        (if (== 0 (rem i p))
          (recur (->> (iterate #(/ % p) i)
                      (take-while integer?)
                      last)
                 (next-prime p)
                 (* res p))
          (recur i (next-prime p) res))))))

(defn hexa
  [n]
  (let [refs (into {} (map #(vector % %2) (range 16)
                           [0 1 2 3 4 5 6 7 8 9 "A" "B" "C" "D" "E" "F"]))]
    (loop [i (int n) res []]
      (if (< i 16)
        (apply str (cons (get refs i) res))
        (recur (quot i 16)
               (cons (get refs (rem i 16)) res))))))

(defn repeating-decimal?
  [a b]
  (loop [n (int a) res []]
    (let [m (quot n b)
          mi (rem n b)]
      (if (== mi 0)
        (- a)
        (if (some #(== mi %) res)
          a
          (recur (* mi 10) (conj res mi)))))))

(defn sum-digits
  [n]
  (loop [p n res (int 0)]
    (if (< p 10)
      (+ p res)
      (recur (quot p 10)
             (+ res (rem p 10))))))














