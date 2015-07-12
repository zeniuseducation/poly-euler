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

(defn ^long sola
  [^long lim]
  (let [primes (boolean-array (+ lim 1) true)
        refs (int-array (range (+ lim 1)))]
    (loop [i (int 2) res (long 0)]
      (if (> i lim)
        res
        (if (aget primes i)
          (do (loop [j (int (* i 2))]
                (if (> j lim)
                  nil
                  (do (aset primes j false)
                      (aset refs j (quot (* (- i 1) (aget refs j)) i))
                      (recur (+ j i)))))
              (recur (+ i 1)
                     (+ res (- i 1))))
          (recur (+ i 1) (+ res (aget refs i))))))))

(defn sol33
  [^long lim]
  (reduce *
          (for [i (range 10 lim)
                j (range (+ 1 i) lim)
                :let [ni (numcol i) nj (numcol j)
                      sni (set ni) snj (set nj)
                      sdi (vec (difference sni snj))
                      sdj (vec (difference snj sni))]
                :when (and (not= 0 (rem i 10))
                           (not= 0 (rem i 11))
                           (not= 0 (rem j 11))
                           (not= 0 (rem j 10))
                           (== 1 (count sdi) (count sdj))
                           (and (not-empty sdi) (not-empty sdj))
                           (== (/ i j) (/ (colnum sdi) (colnum sdj))))]
            (/ i j))))

(defn sol50
  [^long lim]
  (let [bahan (sieve lim)]
    (->> bahan
         (iterate rest)
         (take-while not-empty)
         (map-indexed #(vector % (reductions + %2))))))

(def primes (filter prime? (cons 2 (iterate #(+ 2 %) 3))))

(defn sol50p
  "lim is the maximum limit for the sum and bhn is the upper bound for
  sieve" [^long lim ^long bhn]
  (->> (eduction
        (comp (take-while not-empty)
              (map #(reductions + %))
              (mapcat #(map-indexed (fn [a b] [b (inc a)]) %))
              (filter #(and (prime? (first %))
                            (<= (first %) lim))))
        (iterate rest (sieve bhn)))
       (reduce (fn [[a b] [c d]]
                 (if (> b d) [a b] [c d])))))



(def ltribo
  "Lazy sequence of tribonaci"
  (->> (rest (rest ltribo))
       (map #(+' %1 %2 %3) ltribo (rest ltribo) )
       lazy-seq
       (concat [1 1 1])))



(defn sol216a
  "This one is very very slow" [^long lim]
  (loop [n (long 2) res (int 0)]
    (if (> n lim)
      res
      (if (prime? (- (* 2 n n) 1))
        (recur (+ n 1) (+ 1 res))
        (recur (+ n 1) res)))))

(defn sol216b
  "This one is very very slow" [^long lim]
  (loop [n (long 2) res (int 0)]
    (if (> n lim)
      (- lim res)
      (if (== 0 (rem (- (* 2 n n) 1) 7))
        (recur (+ n 1) (+ 1 res))
        (recur (+ n 1) res)))))

(defn ^longs sol216
  [^long lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))]
    (loop [i (int 3) res (int 0)]
      (if (> i lim)
        res
        (if (aget refs i)
          (do (when (<= i llim)
                (loop [j (int (* i i))]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j (* 2 i)))))))
              (let [n (quot (+ 1 i) 2)]
                (if (psqr? n)
                  (recur (+ i 2) (+ res 1))
                  (recur (+ i 2) res))))
          (recur (+ i 2) res))))))

(defn tribo-mod?
  "Returns true if a number divides any term of tribonacci" [^long n]
  (let [ltribon (iterate #(let [[a b c] %]
                            [(rem (+ a b c) n) a b]) [1 1 1])]
    (loop [[x & xs] (rest ltribon)]
      (if (= x [1 1 1])
        false
        (if (some #(== 0 %) x)
          true
          (recur xs))))))

(defn sol225
  [^long n]
  (loop [i (int 23) ctr (int 0)]
    (if (== ctr n)
      (- i 2)
      (recur (+ i 2) (if (tribo-mod? i) ctr (+ ctr 1))))))

(defn crack
  "Build the wall" [^long width]
  (let [bricks [2 3]]
    (loop [i (int 1) res [[2] [3]]]
      (if (> i (quot width 2))
        (filter #(== width (reduce + %)) res)
        (recur (+ i 1)
               (concat res (for [r res b bricks] (conj r b))))))))

(defn fibopart
  [^long n]
  (loop [i (int 1) res [[1] 1]]
    (if (== i n)
      res
      (let [[a b] res]
        (recur (+ i 1)
               (cons (conj a b) res))))))

(defn sol216
  [^long lim]
  (loop [i (int 4) res 0]
    (let [n (/ (- (* i i) 9) 8)]
      (if (> n lim)
        res
        (if (integer? n)
          (recur (+ i 1) (+ res 1))
          (recur (+ i 1) res))))))

(defn walling
  [^long wid]
  (let [size [2 3]]
    (loop [i (int 1) res [[2] [3]] cres []]
      (if (> i (quot wid 2))
        (filter #(== wid (last %)) cres)
        (let [tmp (for [s size r res
                        :let [lr (last r)
                              st (+ lr s)]
                        :when (<= st wid)]
                    (conj r st))]
          (recur (+ i 1) tmp (concat cres tmp)))))))

(def wall9 (walling 9))

(def find-crack
  (memoize
   (fn [xs n]
     (if (empty? xs)
       0
       (if (== n 1)
         (if (empty? xs) 0 1)
         (loop [[k & ks] xs ki 0 res [[2] [3]]]
           (if ks
             (recur ks
                    k
                    (for [r res i [2 3]
                          :let [lr (last r)]
                          :when (and (not= lr ki) (not= k lr))]
                      (conj r (+ lr i))))
             (->> (filter #(and (not-any? (set (butlast xs)) %)
                                (not-any? #{(- k 1)} %)) res)
                  (map #(let [lr (last %)]
                          (if (== k lr)
                            %
                            (if (> lr k)
                              (conj (vec (take-while (fn [x] (< x k)) %)) k)
                              (conj % k)))))
                  distinct
                  (map #(find-crack % (- n 1)))
                  (reduce +)))))))))


(def find-crack2
  (memoize
   (fn [xs n]
     (if (== n 1)
       1
       (let [tmp (filter #(not-any? (set (butlast xs)) %) wall9)]
         (if (empty? tmp) 0 (reduce + (map #(find-crack2 % (- n 1)) tmp))))))))



(defn cracks
  [^long times ^long wid]
  (let [bahan (walling wid)]
    (loop [i (int 1) res bahan ctr 0]
      (if (== i (- times 1))
        (count (distinct (mapcat find-crack res)))
        (let [tmp (mapcat find-crack res)]
          (recur (+ i 1) (distinct tmp) (count tmp)))))))

(defn pascal
  "Building pascal pyramid, where layer is the nth layer in the
  pyramid"
  [^long layer ^long modi]
  (let [facts (long-array (map #(reduce (fn [a b]
                                          (rem (* a b) modi))
                                        1
                                        (range 1 (+ 1 %))) (range (+ layer 1))))
        binom (fn [n k] (rem (quot (nth facts n)
                                  (* (nth facts k)
                                     (nth facts (- n k))))
                            modi))]
    (map #(binom layer %) (range (+ layer 1)))))

(defn build-pascal
  [^long row ^long modi]
  (->> (iterate #(map (fn [a b] (rem (+ a b) modi))
                      (conj (vec %) 0) (cons 0 %)) [1 1])
       (take row) last))

(defn trinoms
  [lim]
  (loop [[x & xs] [1 2] res (map vector (range (+ lim 1)))]
    (if xs
      (recur xs
             (for [r res i (range 0 (+ 1 (- lim (reduce + r))))]
               (conj r i)))
      (map #(conj % (- lim (reduce + %))) res))))

(def blocks
  (memoize
   (fn [^long blacks]
     (let [min-red 50]
       (cond
        (== blacks 0) 1
        (< blacks min-red) 0
        (== blacks min-red) 1
        :else
        (inc (reduce +
                     (for [red (range min-red (+ 1 blacks))
                           position (range blacks)
                           :let [next-blacks (- blacks red position)]
                           :when (> next-blacks 0)]
                       (+ 1 (blocks (- next-blacks 1)))))))))))

(defn sol115
  [^long lim]
  (let [[first-exceeds]
        (->> (range 100 lim)
             (drop-while #(< (blocks %) lim)))]
    [first-exceeds (blocks first-exceeds)]))




































