(ns euler.prob100-150
  (:require [clojure.set :as cs]))

(load-file "math.clj")

;; Problem no 125

(defn squares
  "When called with no argument, returns an infinitely lazy seq of
  square numbers starting from 1. When called with one arguments,
  returns nth first square numbers starting from 1."
  [& args]
  (if (empty? args)
    (pmap square (iterate inc 1))
    (pmap square (range 1 (inc (first args))))))

(defn sol125
  [n lim]
  (->> (squares n)
       (iterate rest)
       (take n)
       (mapcat #(rest (reductions + %)))
       (filter #(< % lim))
       (filter palin?)
       (distinct)
       (sum)))

(def res125 (atom #{}))

(defn sqr [x] (* x x))

(defn palin?2
  [n]
  (let [tmp (numcol n)]
    (= tmp (reverse tmp))))

(defn squares125
  [lim]
  (loop [i 2 res #{}]
    (let [tmp (sqr (dec i))]
      (if (>= tmp lim)
        (sum res)
        (recur (inc i)
               (cs/union res
                         (loop [n i sumn tmp res1 #{}]
                           (let [tsqr (+ sumn (sqr n))]
                             (if (>= tsqr lim)
                               res1
                               (recur (inc n)
                                      tsqr
                                      (if (palin?2 tsqr)
                                        (conj res1 tsqr)
                                        res1)))))))))))

(defn sol125a
  [lim]
  (time (squares125 lim)))

;; Problem no 110

(defn diop
  "Returns a number of solution for diophantine equation for a given n"
  [n]
  (loop [x (inc n) res 0]
    (if (>= x (* 2 n))
      (inc res)
      (recur (inc x)
             (if (integer? (/ (*' x n) (- x n))) (inc res) res)))))

(defn sol110
  "Returns the first number n in diophantine equation that has more
  than target distinct solutions."
  [target]
  (->> (iterate inc (*' target target))
       (filter #(> (count-divs %) (/ target 2)))
       (map #(vector % (diop %)))
       (drop-while #(<= (second %) target))
       first))

;; Problem no 131

(defn cube?
  [i]
  (loop [n 1]
    (if (> (cube n) i)
      false
      (if (= (cube n) i)
        true
        (recur (inc n))))))

(defn gamski
  [n p]
  (for [i (range 1 n)]
    (+ (cube i)
       (* p (square i)))))

(defn pergamski
  [n lim]
  (for [i (range 1 lim)
        p (primes-under n)
        :let [tmp (+' (cube i)
                     (*' p (square i)))]
        :when (cube? tmp)] [p tmp i]))

;; Problems no 134


(defn prime-con
  [n]
  (loop [p1 5 p2 (next-prime p1) res 0]
    (if (> p1 n)
      res
      (recur p2
             (next-prime p2)
             (+ res
                (loop [i (quot p2 2)]
                  (let [tmp (colnum (concat (numcol i) (numcol p1)))]
                    (if (div? tmp p2)
                      tmp
                      (recur (inc i))))))))))

(defn sol134
  [n]
  (loop [p1 5 p2 (next-prime p1) res []]
    (if (> p1 n)
      res
      (recur p2
             (next-prime p2)
             (conj res
                (loop [i (quot p2 2)]
                  (let [tmp (numcol p1)
                        s (* i p2)]
                    (if (= tmp (take-last (count tmp) (numcol s)))
                      s
                      (recur (inc i))))))))))



