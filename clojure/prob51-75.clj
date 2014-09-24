(ns euler.prob51-75
  (:require [clojure.set :as cs]))

(load-file "math.clj")

;; Problem 66

(defn psqr?
  [n]
  (let [x (Math/sqrt n)]
    (== (bigint x) x)))

(defn qdiop
  [d]
  (loop [y 1]
    (let [xsqr (inc (*' d y y))]
      (if (psqr? xsqr)
        (vector d (bigint (Math/sqrt xsqr)) y)
        (recur (inc y))))))

;; d x y

(defn sol66
  [ds n]
  (time (loop [d ds res [0 0 0]]
          (if (> d n)
            res
            (if (psqr? d)
              (recur (inc d) res)
              (let [[td tx ty] (qdiop d)]
                (if (> (second res) tx)
                  (recur (inc d) res)
                  (recur (inc d) [td tx ty]))))))))

(defn euler66
  [n]
  (sol66 2 n))

;; PROBLEM 69 -> Totient Maximum

(defn candidates
  [lim]
  (let [rawmat (reverse (prime-list 20))]
    (loop [n 1 res []]
      (let [rm (take n rawmat)
            num (product rm)]
        (if (> num lim)
          res
          (recur (inc n) (conj res num)))))))

(defn maxtot
  [lim]
  (time (sort-by second (map totient (candidates lim)))))


;; PROBLEM 75

(defn sqr [x] (* x x ))

(defn triangles                         
  [n]
  (for [a (range 1 (inc (quot n 3)))
        b (range a (* 2 (inc (quot n 3))))
        :let [c (- n a b)]
        :when (= (sqr c)
                 (+ (sqr a) (sqr b)))] (+ a b c)))

(defn tri2
  [n]
  (let [m (quot n 3)]
    (for [a (range 3 m)
          b (range a (inc (let [asqr (inc (quot (- (sqr a) 2) 2))]
                            (if (> asqr (* 2 m))
                              (* 2 m)
                              asqr))))
          :let [c (+ (sqr a) (sqr b))]
          :when (psqr? c)] (+ a b (int (Math/sqrt c))))))

(defn sol75
  [lim]
  (->> (range 3 (+ 2 (quot lim 3)))
       frequencies
       (filter #(= 1 (val %)))
       (map first)
       (filter #(<= % lim))
       count))

;; latest technology

(defn a->bc
  "Consider a^2 + b^2 = c^2, this fn returns the value of a+b+c from a
  given a, or nil if it's impossible to construct integer-value b and
  c from a given a."
  [a lim]
  (let [asqr (sqr a)]
    (loop [b (inc a)]
      (do (if (zero? (rem b 500))
            (println a " " b))
          (if (or (> (* 2 b) asqr)
                  (> (+ a b) (* 2 (/ lim 3))))
            nil
            (if-let [sol (loop [x 1]
                           (let [tmp (* x (+ x (* 2 b)))]
                             (if (or (> tmp asqr)
                                     (> (+ a b (+ b x)) lim))
                               nil
                               (if (= tmp asqr)
                                 (+ a b (+ b x))
                                 (recur (inc x))))))]
              sol
              (recur (inc b))))))))

(defn lazyfor
  [a lim]
  (let [asqr (sqr a)]
    (for [b (iterate inc a)
          :while (or (<= (* 2 b) asqr)
                     (<= (+ a b) (* 2 (/ lim 3))))
          :let [sol (loop [x 1]
                      (let [tmp (* x (+ x (* 2 b)))]
                        (if (or (> tmp asqr)
                                (> (+ a b (+ b x)) lim))
                          nil
                          (if (= tmp asqr)
                            (+ a b (+ b x))
                            (recur (inc x))))))]] sol)))

(defn soulmate
  [lim]
  (->> (inc (quot lim 3))
       (range 3)
       (mapcat #(lazyfor % lim))
       (remove nil?)
       (group-by identity)
       (filter #(= 1 (count (second %))))
       count))

(defn foulmate
  [lim]
  (->> (inc (quot lim 3))
       (range 3)
       (map #(a->bc % lim))
       (remove nil?)
       frequencies
       (filter #(= 1 (val %))) 
       count))

(def base-primes (primes-under 100))

(defn conds70?
  [n]
  (let [tot (totient n)
        ntot (str tot)
        ncol (str n)]
    (if (= (sort ntot) (sort ncol))
      [n (/ n tot)]
      nil)))

(defn cand70
  [lim]
  (->> (range 10 lim)
       (keep conds70?)
       (sort-by second)
       (first)
       (time)))





























