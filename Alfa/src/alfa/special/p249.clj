(ns alfa.special.p249
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

(def primes (sieve 545))

(def checki
  (fn [^long n]
    (cond
      (== n 0) 1
      (< n 0) 0
      (== n 1) 0
      (some #(== n %) primes) 1
      :else (->> (take-while #(<= % n) primes)
                 (drop-while #(<= % (quot n 2)))
                 (map #(checki (- n %)))
                 (reduce +')))))

(defn sol249c
  [^long lim]
  (let [raw (sieve (reduce + (sieve lim)))
        modi (expt 10 15)]
    (loop [[x & xs] raw res 0N]
      (if x
        (recur xs (rem (+ res (checki x)) modi))
        res))))

(defn sol249d
  [^long lim]
  (let [primes (sieve lim)
        modi (expt 10 16)
        llim (reduce + (sieve lim))
        lllim (int (Math/sqrt llim))
        refs (boolean-array (inc llim) true)]
    (do (doseq [i (range 3 (inc lllim) 2)
                :when (aget refs i)]
          (doseq [j (range (* i i) (inc llim) i)]
            (aset refs j false)))
        (loop [[x & xs] primes sum (bigint (count primes)) npr {}]
          (if x
            (let [nprs (->> (map #(+ x %) (keys npr))
                            (map #(vector % (npr (- % x))))
                            (into {})
                            (merge-with + {x 1}))
                  nprss (merge-with +' npr nprs)
                  resi (for [i (filter even? (keys nprs)) j xs
                             :when (aget refs (+ i j))] i)
                  reso (reduce +' (map nprs resi))]
              (do (println x)
                  (recur xs
                         (rem (+' sum reso) modi)
                         nprss)))
            sum)))))

(defn sol249f
  [^long lim]
  (let [primes (sieve lim)
        modi (expt 10 16)]
    (loop [[x & xs] primes sum (bigint (count primes)) npr {}]
      (if x
        (let [nprs (->> (map #(+ x %) (keys npr))
                        (map #(vector % (npr (- % x))))
                        (into {})
                        (merge-with + {x 1}))
              nprss (merge-with +' npr nprs)
              resi (for [i (filter even? (keys nprs)) j xs
                         :when (odd-prime? (+ i j))] i)
              reso (reduce +' (map nprs resi))]
          (do (println x)
              (recur xs
                     (rem (+' sum reso) modi)
                     nprss)))
        sum))))

(defn sol249e
  [^long lim]
  (let [primes (sieve lim)
        modi (expt 10 15)]
    (loop [[x & xs] primes sum 0N npr []]
      (if x
        (let [nprs (cons x (map #(+ x %) npr))
              resi (for [i (filter even? nprs) j xs
                         :let [s (+ i j)]
                         :when (odd-prime? s)] 1)
              reso (reduce + resi)]
          (do (println reso sum)
              (recur xs (rem (+ sum reso) modi) (concat npr nprs))))
        (+ (count primes) sum)))))


