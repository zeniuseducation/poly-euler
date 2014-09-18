(ns euler.random
  (:require [clojure.set :as cs]))

(load-file "math.clj")

(defn sometest
  "n as n-th hamming, lim as upper limit"
  [n lim]
  (distinct (mapcat #(range % lim %) (primes-under n))))

(defn primes [n] (primes-under n))

(defn firstlist
  [ls lim]
  (-> #(for [j (iterate inc 1)
             :let [tmp (expt % j)]
             :while (<= tmp lim)] tmp)
      (mapcat ls)
      (sort)))

(defn sol
  [n lim]
  ())

(defn genham
  [ls n lim]
  (->> (iterate #(sort (distinct (lazy-cat %
                                      (for [i % j %
                                            :let [res (*' i j)]
                                            :while (<= res lim)] res))))
                ls)
       (take n)
       (last)))

(defn expit
  [lim ls]
  (loop [x ls res []]
    (if (empty? x)
      (sort (distinct res))
      (recur (rest x)
             (concat res
                     (loop [i 1 res1 []]
                       (let [tmp (expt (first x) i)]
                         (if (> tmp lim)
                           res1
                           (recur (inc i) (conj res1 tmp))))))))))

(defn timesit
  [lim ls]
  (loop [x ls res []]
    (if (empty? x)
      (sort (distinct res))
      (if (>= (* 2 (first x)) lim)
        (sort (distinct res))
        (recur (rest x)
               (concat res
                       (loop [x1 ls res1 []]
                         (if (empty? x1)
                           res1
                           (let [tmp (* (first x) (first x1))]
                             (if (> tmp lim)
                               res1
                               (recur (rest x1) (conj res1 tmp))))))))))))

(defn soltemp
  [ls lim]
  (let [ep (expit lim ls)]
    (sort (distinct (concat ep (timesit lim ep))))))

(defn solone
  [n lim]
  (loop [i 1 res (primes 100)]
    (if (>= i n)
      res
      (recur (inc i)
             (sort (distinct (concat res (soltemp res lim))))))))


;; Some stuffs






