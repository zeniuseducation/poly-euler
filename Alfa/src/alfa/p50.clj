(ns alfa.p50
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :as cs]
   [alfa.common :refer :all])
  (:import (java.util Vector)))

(defn ^long sol1
  [^long lim]
  (let [[a b c] (pvalues (apply + (range 3 lim 3))
                         (apply + (range 5 lim 5))
                         (apply + (range 15 lim 15)))]
    (- (+ a b) c)))

(defn ^long sol1a
  [^long lim]
  (loop [i (int 1) res (int 0)]
    (if (>= i lim)
      res
      (if (or (== 0 (rem i 3))
              (== 0 (rem i 5)))
        (recur (+ i 1) (+ i res))
        (recur (+ i 1) res)))))

(defn ^long sol5
  [^long lim]
  (let [refs (int-array (range (+ lim 1)))]
    (loop [i (int 2) res (int 1)]
      (if (> i lim)
        res
        (let [tmpi (aget refs i)]
          (do (loop [j (int (+ i 1))]
                (if (> j lim)
                  nil
                  (let [tmpj (aget refs j)]
                    (if (== 0 (rem tmpj tmpi))
                      (do (aset refs j (quot tmpj tmpi))
                          (recur (+ j 1)))
                      (recur (+ j 1))))))
              (recur (+ i 1) (* res tmpi))))))))

(defn ^long sol9
  [^long lim]
  (loop [m (int 2)]
    (if (> m (quot lim 2))
      nil
      (let [msqr (* m m)]
        (if-let [res (loop [n (int 1)]
                       (if (>= n m)
                         nil
                         (let [nsqr (* n n)
                               a (- msqr nsqr)
                               b (* 2 m n)
                               c (+ msqr nsqr)
                               peri (+ a b c)]
                           (if (> peri lim)
                             nil
                             (if (== 0 (rem lim peri))
                               (* (expt (quot lim peri) 3) (* a b c))
                               (recur (+ n 1)))))))]
          res
          (recur (+ m 1)))))))

(def bahan18
  (->> (cs/split-lines (slurp "resources/p18.txt"))
       (map #(cs/split % #" "))
       (mapv #(mapv (fn [[x & xs :as n]]
                      (if (= \0 x)
                        (read-string (apply str xs))
                        (read-string n))) %))))

(def bahan67
  (->> (cs/split-lines (slurp "resources/p67.txt"))
       (map #(cs/split % #" "))
       (mapv #(mapv (fn [[x & xs :as n]]
                      (if (= \0 x)
                        (read-string (apply str xs))
                        (read-string n))) %))))

(def sol18
  (memoize
   (fn [a b]
     (if (== a 14)
       (get-in bahan18 [a b])
       (if (> b a)
         0
         (+ (get-in bahan18 [a b])
            (max (sol18 (+ a 1) b)
                 (sol18 (+ a 1) (+ b 1)))))))))

(def sol67
  (memoize
   (fn [a b]
     (if (== a 99)
       (get-in bahan67 [a b])
       (if (> b a)
         0
         (+ (get-in bahan67 [a b])
            (max (sol67 (+ a 1) b)
                 (sol67 (+ a 1) (+ b 1)))))))))

(defn lazy-sieve
  ([^long lim]
     (->> (iterate #(+ % 2) 5)
          (lazy-sieve 3)
          (cons 2)
          (take-while #(< % lim))))
  ([a xs]
     (->> (rest xs)
          (filter #(not= 0 (rem % a)))
          (lazy-sieve (first xs))
          (lazy-seq)
          (cons a))))

(defn collatz
  ([^long n]
     (if (== n 1)
       1
       (collatz n 0)))
  ([^long n ^long res]
     (if (== n 1)
       (+ 1 res)
       (if (even? n)
         (collatz (quot n 2) (+ res 1))
         (collatz (+ 1 (* 3 n)) (+ res 1))))))

(defn sol14
  [^long lim]
  (loop [i (int 500001) res (int 0) resn (int 0)]
    (if (> i lim)
      resn
      (let [tmp (collatz i)]
        (if (> tmp res)
          (recur (+ i 2) tmp i)
          (recur (+ i 2) res resn))))))

(defn sol28
  [^long lim]
  (let [spirals (fn [^long n]
                  (reduce + (take 4 (iterate #(- % (- n 1)) (* n n)))))]
    (+ 1 (reduce + (map spirals (range 3 (+ lim 1) 2))))))

(defn sol13
  []
  (let [raw (slurp "resources/p13.txt")]
    (->> (cs/split-lines raw)
         (map bigint)
         (reduce +)
         str
         (take 10)
         (apply str))))

(defn gen-pascal
  [lim]
  (->> [1]
       (iterate #(mapv + (cons 0 %)
                       (conj % 0)))
       (take lim)))

(defn sol15
  [size]
  (->> [1]
       (iterate #(mapv + (cons 0 %) (conj % 0)))
       (drop size)
       first
       (map #(* % %))
       (reduce +)))

(defn sol16
  [n]
  (sum-digits (expt 2 n)))

(defn proper-not-proper
  [num]
  (let [facs (filter #(== 0 (rem num %)) (range 1 (+ 1 (int (Math/sqrt num)))))]
    (distinct (concat facs (map #(quot num %) (rest facs))))))

(defn sumproper
  [^long x]
  (if (== 0 (rem x 2))
    (loop [i (int 2) res (int 1)]
      (if (>= (* i i) x)
        (if (> (* i i) x)
          res
          (+ i res))
        (recur (+ i 1)
               (if (== 0 (rem x i))
                 (+ res i (quot x i))
                 res))))
    (loop [i (int 3) res (int 1)]
      (if (>= (* i i) x)
        (if (> (* i i) x)
          res
          (+ i res))
        (recur (+ i 2)
               (if (== 0 (rem x i))
                 (+ res i (quot x i))
                 res))))))

(defn sol33
  [^long lim]
  (->> (for [i (range 10 lim)
             j (range (+ 1 i) lim)
             :let [ni (numcol i)
                   nj (numcol j)
                   nir (remove (set nj) ni)
                   njr (remove (set ni) nj)
                   cnjr (colnum njr)]
             :when (and (not= 0 (rem i 10))
                        (not= 0 (rem j 10))
                        (not= ni nir)
                        (not= nj njr)
                        (if (or (= njr [0])
                                (= njr [])
                                (= nir []))
                          false
                          (== (/ i j) (/ (colnum nir) cnjr))))]
         (/ i j))
       (reduce *)
       denominator))

(defn sol46
  []
  (loop [i (int 9)]
    (if (prime? i)
      (recur (+ i 2))
      (let [tmp (loop [j (int 1)]
                  (let [tmpj (* 2 j j)]
                    (if (> tmpj i)
                      false
                      (if (prime? (- i tmpj))
                        true
                        (recur (+ j 1))))))]
        (if tmp (recur (+ i 2)) i)))))

(defn sol48
  [^long lim ^long modi]
  (rem (transduce
        (comp (map #(modex % % modi)))
        + (range 1 lim)) modi))

(defn abundants
  [^long lim]
  (let [abuns (boolean-array (+ lim 1))
        sumabuns (boolean-array (+ lim 1) true)]
    (do (loop [i (int 1)]
          (if (>= i lim)
            nil
            (do (let [propi (sumproper i)]
                  (aset abuns i (> propi i)))
                (recur (+ i 1)))))
        (loop [i (int 12) res (long (quot (* lim (- lim 1)) 2))]
          (if (>= i (+ 1 (quot lim 2)))
            res
            (if (aget abuns i)
              (let [tmp (loop [j (int i) resj (int 0)]
                          (let [jum (+ j i)]
                            (if (>= jum lim)
                              resj
                              (if (aget abuns j)
                                (if (aget sumabuns jum)
                                  (do (aset sumabuns jum false)
                                      (recur (+ j 1) (+ resj jum)))
                                  (recur (+ j 1) resj ))
                                (recur (+ j 1) resj)))))]
                (recur (+ i 1) (- res tmp)))
              (recur (+ i 1) res)))))))

































