(ns alfa.special.p277
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

(defn next-col
  [^long n]
  (cond (== 0 (rem n 3)) ["D" (quot n 3)]
        (== 1 (rem n 3)) ["U" (quot (+ 2 (* 4 n)) 3)]
        (== 2 (rem n 3)) ["d" (quot (- (* 2 n) 1) 3)]))

(defn rev-col
  [chr n]
  (cond (= \D chr) (* 3 n)
        (= \d chr) (quot (+ 1 (* 3 n)) 2)
        (= \U chr) (quot (- (* 3 n) 2) 4)))

(defn get-num
  [st start]
  (loop [[x & xs] (reverse st) res start]
    (if x
      (recur xs (rev-col x res))
      res)))

(def collatz
  (fn [^long n]
    (if (== n 1)
      ""
      (let [[st num] (next-col n)
            sts (collatz num)]
        (str st sts)))))

(defn sol187
  [^long lim]
  (let [raws (sieve (quot lim 2))
        ctr (count raws)
        primes (int-array raws)
        llim (int (Math/sqrt lim))]
    (loop [i (int 0) j (- (int ctr) 1) res (int 0)]
      (let [pi (aget primes i)]
        (if (> pi llim)
          res
          (let [[resj k] (loop [k j]
                           (let [pj (aget primes k)
                                 tmp (* pi pj)]
                             (if (< tmp lim)
                               [(+ 1 (- k i)) k]
                               (recur (- k 1)))))]
            (recur (+ i 1) k (+ res resj))))))))

(defn sol512
  [^long lim]
  (let [primes (int-array (sieve lim))
        ctr (count primes)
        tots (int-array (->> (range (+ lim 2))
                             (map #(if (even? %) 0 %))))]
    (loop [i (int 1)]
      (if (< i ctr)
        (let [p (aget primes i)]
          (do (aset tots p (- p 1))
              (loop [j (* 3 p)]
                (when (<= j lim)
                  (do (aset tots j (quot (* (aget tots j) (- p 1)) p))
                      (recur (+ j (* 2 p))))))
              (when (== 0 (rem i 100000))
                (println p))
              (recur (+ i 1))))
        (transduce
          (map #(aget tots %))
          + (range 1 (+ lim 1) 2))))))

(defn sol512b
  [^long lim]
  (let [primes (boolean-array (+ lim 1) true)
        llim (long (Math/sqrt lim))
        tots (long-array (range (+ lim 2)))]
    (loop [i (int 3) res (long 1)]
      (if (<= i lim)
        (let [p (aget primes i)]
          (if p
            (do (when (<= i llim)
                  (loop [j (long (* i i))]
                    (when (<= j lim)
                      (aset primes j false)
                      (recur (+ j i i)))))
                (loop [j (* 3 i)]
                  (when (<= j lim)
                    (do (aset tots j (quot (* (aget tots j) (- i 1)) i))
                        (recur (+ j (* 2 i))))))
                (when (== 0 (rem i 100000))
                  (println i))
                (recur (+ i 2) (+ res (- i 1))))
            (recur (+ i 2) (+ res (aget tots i)))))
        res))))

(defn colat
  [^long n st]
  (let [ctr (count st)]
    (loop [i n c (int 0) res ""]
      (if (>= c ctr)
        true
        (if (not= res (apply str (take c st)))
          false
          (let [[s num] (next-col i)]
            (recur num (+ c 1) (apply str res s))))))))

(defn soltol3
  [^long start ^String st]
  (loop [i start]
    (if (colat i st)
      i
      (do (when (== 0 (rem i 100000))
            (println i))
          (recur (+ i 1))))))


(defn soltol2
  [start]
  (let [st "UDDDUdddDDUDDddDdDddDDUDDdUUDd"
        ctr (count st)]
    (loop [i (+ start 1)]
      (let [sts (->> (collatz i)
                     (take ctr)
                     (apply str))]
        (if (= sts st)
          [i sts st]
          (do (when (== 0 (rem i 10000))
                (println (- i start)))
              (recur (+ i 1))))))))

(defn soltol1
  [lim llim]
  (let [st "UDDDUdddDDUDDddDdDddDDUDDdUUDd"]
    (loop [i 20365000 j (int 0) res []]
      (let [num (get-num st i)]
        (if (> j llim)
          (apply min res)
          (if (> num lim)
            (do (println i j)
                (recur (+ i 1) (+ j 1) (conj res num)))
            (recur (+ i 1) j res)))))))
