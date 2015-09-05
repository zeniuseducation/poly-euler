(ns alfa.p350
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

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

(defn bigdec-expt
  [a m]
  (cond (== m 0) 1M
        (== m 1) (bigdec a)
        :else (let [nex (bigdec-expt a (quot m 2))]
                (* nex nex (if (even? m) 1 (bigdec a))))))

(defn fibo-mod
  [^long i modi]
  (cond (== i 0) 0
        (== i 1) 1
        :else (let [sqrt5 (Math/sqrt 5)
                    Phi (/ (+ 1 sqrt5) 2)
                    phi (/ (- 1 sqrt5) 2)]
                (-> (/ (- (bigdec-expt Phi i) (bigdec-expt phi i)) sqrt5)
                    (rem modi)
                    long))))

(defn big-fibo
  [n modi]
  (loop [i (long 1) a (long 1) b (long 0)]
    (if (== i n) a (recur (+ i 1) (rem (+ a b) modi) a))))

(defn sol304
  [lim foo mul]
  (let [size (+ lim (* foo mul))
        modi 1234567891011
        llim (long (sqrt size))
        refs (int-array (sieve llim))
        rsize (count refs)
        primes (boolean-array (* foo mul) true)
        pget (fn [idx]
               (aget primes (- idx lim)))
        pset (fn [idx value]
               (aset primes (- idx lim) value))
        uprimes (loop [i 1]
                  (if (>= i rsize)
                    (->> (range (+ lim 1) size 2)
                         (filter #(pget %))
                         long-array)
                    (let [prime (aget refs i)
                          start (let [tmp (rem lim prime)
                                      itmp (+ lim (- prime tmp))]
                                  (if (even? itmp) (+ prime itmp) itmp))]
                      (do (loop [j start]
                            (when (< j size)
                              (pset j false)
                              (recur (+ j prime prime))))
                          (recur (+ i 1))))))]
    (loop [i (int 0) res (long 0)]
      (if (> i foo)
        res
        (recur (+ i 1)
               (rem (+ res (fibo-mod (aget uprimes i) modi)) modi))))))

(def fibo-bego
  (memoize
    (fn [i]
      (cond (== i 0) 0
            (== i 1) 1
            :else (+ (fibo-bego (- i 1))
                     (fibo-bego (- i 2)))))))

(def matrix
  (->> (slurp "resources/p18.txt")
       split-lines
       (map #(cs/split % #" "))
       (mapv #(mapv (fn [x] (Integer/parseInt x)) %))))

(defn maxi
  [i j]
  (if (get-in matrix [(inc i)])
    (+ (get-in matrix [i j])
       (max (maxi (+ i 1) j)
            (maxi (+ i 1) (+ j 1))))
    (get-in matrix [i j])))






