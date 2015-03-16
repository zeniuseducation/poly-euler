(ns alfa.four
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all])
  (:import (clojure.lang BigInt)))

(def sol
  (memoize
   (fn [i cl lim cur]
     (let [nums (map #(conj cur %) (if (== cl 1) [:a :o] [:a :l :o]))
           ncur (->> nums
                     (map #(vec (rest %)))
                     (filter #(not-every? #{:a} %)))]
       (if (== i (- lim 1))
         (count ncur)
         (->> ncur
              (map #(sol (+ i 1)
                         (if (== cl 1) cl (if (some #{:l} %) 1 0))
                         lim %))
              (reduce +')))))))

(defn ^long tsol
  [^long lim]
  (->> (permutes [:a :l :o] 3)
       (filter #(<= (count (filter (fn [x] (= :l x)) %)) 1))
       (filter #(not-every? #{:a} %))
       (pmap #(sol 3 (if (some #{:l} %) 1 0) lim %))
       (reduce +')))

(defm fibo
  [i]
  (cond (== i 1) 1
        (== i 2) 2
        :else (+' (fibo (- i 1)) (fibo (- i 2)))))

(def refs (make-array BigInt 100))

(def zeck
  (fn [n]
    (let [x (->> (iterate inc 1)
                 (drop-while #(<= (aget refs %) n))
                 first dec)
          num (aget refs x)]
      (if (== num n) 1 (+ 1 (zeck (- n num)))))))

(defn solp
  [lim]
  (do (aset refs 1 1N)
      (aset refs 2 2N)
      (loop [i (int 3) a 2N b 1N]
        (if (> i 99)
          (map #(aget refs %) (range 1 i))
          (recur (+' i 1)
                 (aset refs i (+' a b))
                 a)))
      (->> (pmap #(zeck %) (range 1 lim))
           (reduce +))))

(defn soln
  [lim]
  (let [refs (make-array BigInt 100)
        fibos (->> (iterate inc 1)
                   (map #(aget refs %))
                   (take-while #(< % lim)))
        zecks (memoize
               (fn zecks [^long n]
                 (if (<= n 2)
                   [1 1]
                   (let [xs (map #(zecks %) (range (- n 2) 0 -1))]
                     [(+ 1 (reduce + (map first xs)))
                      (+ 1 (reduce + (map #(+ %1 %2) (map first xs) (map second xs))))]))))
        lzeck (memoize
               (fn lzeck
                 [llim]
                 (if (<= llim 2)
                   [1 1]
                   (let [fibs (->> (iterate inc 1)
                                   (map #(aget refs %))
                                   (take-while #(<= % llim)))]
                     (loop [[x & xs] fibs i (int 1) res []]
                       (if xs
                         (let [nms (zecks i)]
                           (recur xs (+ i 1) (conj res [(first nms) (second nms)])))
                         (let [lz (lzeck (- llim x))]
                           [(+ 1 (first lz) (reduce + (map first res)))
                            (+ 1 (first lz) (second lz) (reduce + (map #(+ (first %)
                                                                           (second %)) res)))])))))))]
    (do (aset refs 1 1N)
        (aset refs 2 2N)
        (loop [i (int 3) a 2N b 1N]
          (if (> i 99)
            (map #(aget refs %) (range 1 i))
            (recur (+' i 1)
                   (aset refs i (+' a b))
                   a)))
        (loop [[x & xs] fibos i (int 1) res 0]
          (if xs
            (recur xs (+ 1 i) (+ res (second (zecks i))))
            (let [lz (lzeck (- lim x))]
              (+ res (second lz))))))))

(def maxi (atom 0))

(defn solna
  [lim n]
  (if (<= lim 2)
    (+ n n)
    (let [refs (make-array BigInt 100)]
      (do (aset refs 1 1N)
          (aset refs 2 2N)
          (loop [i (int 3) a 2N b 1N]
            (if (> i 99)
              nil
              (recur (+' i 1)
                     (aset refs i (+' a b))
                     a)))
          (let [fibos (->> (iterate inc 1)
                           (map #(aget refs %))
                           (take-while #(<= % lim)))
                fibis (loop [[x & xs] (rest (butlast fibos)) res [[1 1] [1 1]]]
                        (if xs
                          (recur xs (conj res [x (+ (reduce + (last (butlast res)))
                                                    (second (last res)))]))
                          res))]
            (if (some #{lim} fibos)
              (+ (* n (reduce + (map first fibis)))
                 (reduce + (map second fibis))
                 (inc n))
              (+ (+ (* n (reduce + (map first fibis)))
                    (reduce + (map second fibis)))
                 (inc n)
                 (solna (- lim (last fibos)) (+ 1 n)))))))))


(defn pita
  [lim]
  (reduce +
          (for [a (range 3 (inc (quot lim 4)))]
            (reduce +
                    (for [b (range (+ a 1) (inc (quot lim 2)))
                          :let [c (+ (* a a) (* b b))
                                cs (Math/sqrt c)
                                csq (int cs)]
                          :when (and (<= (+ a b csq) lim)
                                     (== cs csq)
                                     (> csq b))] 1)))))

(defn ^long repeating
  [^long lim]
  (let [refs (vec (reverse (sieve lim)))]
    (loop [[x & xs] refs [r rs] [x (int 0)]]
      (if (< x rs)
        r
        (let [[tm1 tm2]
              (loop [diva (int 1000) res (int 0) xs []]
                (let [t2 (rem diva x)]
                  (if (some #(== t2 %) xs)
                    [x res]
                    (recur (* 10 t2) (+ 1 res) (conj xs t2)))))]
          (if (> rs tm2)
            (recur xs [r rs])
            (recur xs [tm1 tm2])))))))

(defn count-comb
  [n k]
  (quot (reduce * (range (+ k 1) (+ 1 n)))
        (* (reduce * (range 1 (+ k 1)))
           (reduce * (range 1 (+ 1 (- n k)))))))

(def memcomb
  (memoize
   (fn [n k]
     (count-comb n k))))

(defn prob493
  [^long m ^long k]
  (let [res (for [a (range (inc k))
                  b (range (inc k))
                  c (range (inc k))
                  d (range (inc k))
                  e (range (inc k))
                  f (range (inc k))
                  g (range (inc k))
                  :when (== m (+ a b c d e f g))
                  :let [resi (->> [a b c d e f g]
                                  (filter #(> % 0)))]]
              (* (count resi)
                 (transduce
                  (map #(memcomb 10 %))
                  *' resi)))]
    (/ (reduce + res) 1.0)))

(defn ^boolean abun?
  [^long m]
  (if (even? m)
    (loop [i (int 2) res (int 1)]
      (cond
       (> res m) true
       (> (* i i) m) false
       (== (* i i) m) (> (+ res i) m)
       (zero? (rem m i)) (recur (+ 1 i)
                                (+ res i (quot m i)))
       :else (recur (+ 1 i) res)))
    (loop [i (int 3) res (int 1)]
      (cond
       (> res m) true
       (> (* i i) m) false
       (== (* i i) m) (> (+ res i) m)
       (zero? (rem m i)) (recur (+ 2 i)
                                (+ res i (quot m i)))
       :else (recur (+ 2 i) res)))))

(defn ^long non-abundant-sum
  [^long lim]
  (let [abuns (boolean-array lim)
        sum-abuns (boolean-array lim)]
    (do (loop [i (int 12)]
          (if (< i lim)
            (recur (do (if (abun? i)
                         (aset abuns i true))
                       (+ 1 i)))
            i))
        (loop [i (int 12)]
          (if (< i (quot lim 2))
            (recur (do (if (aget abuns i)
                         (loop [j (int i)]
                           (if (< (+ i j) lim)
                             (recur (do (if (aget abuns j)
                                          (aset sum-abuns (+ i j) true))
                                        (+ 1 j))))))
                       (+ 1 i)))
            i))
        (->> (range 1 lim)
             (filter #(not (aget sum-abuns %)))
             (r/fold +)))))

(defn ^long sol23
  [^long lim]
  (let [abuns (boolean-array (+ 1 lim))
        sum-abuns (boolean-array (+ 1 lim))]
    (do (loop [i (int 12)]
          (if (< i lim)
            (recur (do (if (abun? i)
                         (aset abuns i true))
                       (+ 1 i)))
            i))
        (loop [i (int 12) res (int (quot (* lim (+ lim 1)) 2))]
          (if (< i (quot lim 2))
            (if (aget abuns i)
              (let [tmp (loop [j (int i) resj (int 0)]
                          (if (> (+ i j) lim)
                            resj
                            (if (aget abuns j)
                              (if (aget sum-abuns (+ i j))
                                (recur (+ j 1) resj)
                                (do (aset sum-abuns (+ i j) true)
                                    (recur (+ j 1) (+ resj (+ i j)))))
                              (recur (+ j 1) resj))))]
                (recur (+ i 1) (- res tmp)))
              (recur (+ 1 i) res))
            res)))))

























