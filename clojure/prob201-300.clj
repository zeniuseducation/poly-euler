(ns euler.prob201-300
  (:require [clojure.set :as cs]))


(load-file "math.clj")

;; Problem 211

(declare resil)

(defn sol211a
  [n lim]
  (->> (iterate inc 1)
       (map #(* n %))
       (map #(vector % (resil %)) )
       (drop-while #(>= (second %) lim))
       (first)))


(defn hdivs
  "Return the number of divisors of n"
  [n]
  (if (= n 1)
    1
    (let [plim (inc (Math/sqrt n))]
      (if (even? n)
        (loop [i 2 lim (quot n 2) res 2]
          (if (or (> i plim) (>= i lim))
            (if (= 4 n) 3 res)
            (if (zero? (rem n i))
              (let [tmp (quot n i)]
                (if (= tmp i)
                  (inc res)
                  (recur (inc i) tmp (+ 2 res))))
              (recur (inc i) lim res))))
        (loop [i 3 lim (quot n 3) res 2]
          (if (or (> i plim) (>= i lim))
            res
            (if (zero? (rem n i))
              (let [tmp (quot n i)]
                (if (= tmp i)
                  (inc res)
                  (recur (+ 2 i) tmp (+ 2 res))))
              (recur (+ 2 i) lim res))))))))

;; problem 211

(defn sqr [x] (* x x))

(defn divs-squared
  "Return the number of divisors of n"
  [n]
  (if (= n 1)
    1
    (let [plim (inc (Math/sqrt n))]
      (if (even? n)
        (loop [i 2 lim (quot n 2) res (inc (sqr n))]
          (if (or (> i plim) (>= i lim))
            (if (= 4 n) 5 res)
            (if (zero? (rem n i))
              (let [tmp (quot n i)]
                (if (= tmp i)
                  (+ res (sqr i))
                  (recur (inc i) tmp (+ (sqr i) (sqr tmp) res))))
              (recur (inc i) lim res))))
        (loop [i 3 lim (quot n 3) res (inc (sqr n))]
          (if (or (> i plim) (>= i lim))
            res
            (if (zero? (rem n i))
              (let [tmp (quot n i)]
                (if (= tmp i)
                  (+ (sqr i) res)
                  (recur (+ 2 i) tmp (+ (sqr i) (sqr tmp) res))))
              (recur (+ 2 i) lim res))))))))

(defn sol211
  [lim]
  (->> (range 1 (inc lim))
       (filter #(psqr? (divs-squared %)))
       (sum)))

;; problem 234

(defn semidiv?
  [t a b]
  (not= (div? t a) (div? t b)))

(defn sol234
  [lim]
  (loop [p1 2 p2 3 res 0]
    (let [sp1 (sqr p1) sp2 (sqr p2)]
      (if (> sp2 lim)
        (+ res (let [mp1 (->> (+ p1 sp1)
                              (iterate #(+ % p1))
                              (take-while #(<= % lim))
                              (into #{}))
                     mp2 (->> (- sp2 p2)
                              (iterate #(- % p2))
                              (take-while #(> % sp1))
                              (take-while #(<= % lim))
                              (into #{}))
                     res1 (cs/intersection mp1 mp2)]
                 (sum (cs/difference (cs/union mp1 mp2) res1))))
        (recur p2
               (next-prime p2)
               (+ res (let [mp1 (->> (+ p1 sp1)
                                     (iterate #(+ % p1))
                                     (take-while #(< % sp2)))
                            mp2 (->> (- sp2 p2)
                                     (iterate #(- % p2))
                                     (take-while #(> % sp1)))
                            res1 (concat mp1 mp2)]
                        (- (sum res1) (* 2 p1 p2)))))))))




;; PROBLEM 243

(def base-primes (primes-under 1000))
(def target 15499/94744)

(def useful-primes (primes-under 25))

(defn rtotient
  [n]
  (let [pfacts (pfactors n)]
    [n (/ (* n (product (map #(- 1 (/ 1 %)) pfacts))) (dec n))]))

(defn prime-target
  [lim]
  (time
   (loop [n 2]
     (let [raw (take n base-primes)
           num (product raw)
           res (second (rtotient num))]
       (do (println [num res])
           (if (< res lim)
             [n res]
             (recur (inc n))))))))



(defn resil
  [n]
  (second (totient n)))


