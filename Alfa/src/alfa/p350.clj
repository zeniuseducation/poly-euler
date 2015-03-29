(ns alfa.p350
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all]))

(defn smallest-multiple
  [^long n]
  (let [multis (map #(* n %) (range 10))]
    (loop [i (int 1) res multis]
      (let [tmp (filter #(every? #{0 1 2} (numcol %)) (rest res))]
        (if (empty? tmp)
          (let [nexi (for [r res m multis
                           :let [rm (+ (* (expt 10 i) m) r)]
                           :when (->> (numcol rm)
                                      (take-last i)
                                      (every? #{0 1 2}))] rm)]
            (recur (+ i 1) nexi))
          (apply min tmp))))))

(def special-cases
  (let [bahan [9 90 900 9000 99 990 9900 999 9990 9999]
        muls [12222 122220 1222200 12222000
              1122222222 11222222220 112222222200
              111222222222222 1112222222222220
              11112222222222222222]]
    (reduce + (map #(quot %1 %2) muls bahan))))

(defn sol303
  [^long lim]
  (->> (range 1 (+ lim 1))
       (remove #{9 90 900 9000 99 990 9900 999 9990 9999})
       (pmap #(quot (smallest-multiple %) %))
       (reduce +)
       (+ special-cases)))

(defn max-expt
  [prime lim]
  (transduce
   (comp (map #(expt prime %))
         (take-while #(<= % lim))
         (map #(quot lim %)))
   + (iterate inc 1)))

(defn sol429
  [lim]
  (let [modi 1000000009
        max-primes
        (long-array
         (let [primes (boolean-array (+ lim 1) true)
               llim (long (Math/sqrt lim))]
           (loop [i (long 2)
                  res (transient [])]
             (if (> i lim)
               (persistent! res)
               (if (aget primes i)
                 (if (<= i llim)
                   (do (loop [j (int (* i i))]
                         (when (<= j lim)
                           (aset primes j false)
                           (recur (+ i j))))
                       (recur (+ i 1)
                              (conj! res
                                     (modex i (max-expt i lim) modi))))
                   (recur (+ i 1)
                          (conj! res
                                 (modex i (max-expt i lim) modi))))
                 (recur (+ i 1) res))))))
        ctr (dec (count max-primes))]
    (loop [i (long 0) sum (long 1)]
      (if (> i ctr)
        sum
        (recur (+ i 1)
               (modmul sum
                       (+ 1 (expt (aget max-primes i) 2))
                       modi))))))

(defn solates
  [n]
  (let [bahan (divisors n)
        ctr (quot (count bahan) 2)
        front (take ctr bahan)
        back (reverse (take-last ctr bahan))]
    (sequence
     (filter #(let [[a b] %] (== 1 (gcd a b))))
     (map vector front back))))



