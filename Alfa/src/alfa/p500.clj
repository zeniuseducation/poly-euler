(ns alfa.p500
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all])
  (:import (clojure.lang BigInt)))

(defn kv-log
  "Returns the log version of kv"
  [amap]
  (reduce +' (map #(* (val %) (logbase 2 (key %))) amap)))

(defn highly-divisible
  [tar loopi]
  (let [modi 500500507
        target tar
        maxi (+ target 10)
        primes (sieve 1000)
        calc (fn [xs]
               (let [bahan (sort-by key > xs)]
                 (loop [[k & ks] bahan res (long 1) prs primes]
                   (if k
                     (recur ks
                            (->> (take (val k) prs)
                                 (map #(modex %
                                              (quot (dec (key k)) 2)
                                              modi))
                                 (reduce *') 
                                 (*' res))
                            (drop (val k) prs))
                     (mod res modi)))))]
    (loop [res #{{3 1}} i (int 0)]
      (if (> i loopi)
        (->> (map #(vector (kv-log %) %) res)
             (filter #(<= (- target 0.002) (first %) maxi))
             (map #(vector (second %) (calc (second %))))
             (min-by second))
        (recur (->> (for [r res]
                      (->> r
                           (map #(->> {(+ 2 (key %)) 1 (key %) -1}
                                      (merge-with + r)))
                           (cons (merge-with + r {3 1}))
                           (map #(if (some (fn [x] (<= (val x) 0)) %)
                                   (->> (filter (fn [x]
                                                  (<= (val x) 0)) %)
                                        (map key)
                                        (apply dissoc %))
                                   %))))
                    (apply concat)
                    (concat (->> (map #(vector (kv-log %) %) res)
                                 (filter #(<= (- target 0.002) (first %) maxi))
                                 (map #(vector (second %) (calc (second %))))
                                 (map first)))
                    set)
               (+ i 1))))))

(defn insert
  [x xs]
  (concat (take-while #(<= % x) xs)
          [x]
          (drop-while #(<= % x) xs)))

(declare insert-by)


(defn search
  [target]
  (let [modi 500500507
        primes (map #(vector (logbase 2 %) % 1) (sieve 10000))]
    (loop [res {} prs primes state (long 0)]
      (if (>= state target)
        (reduce #(mod (*' %1 %2) modi)
                (map #(modex (key %) (dec (val %)) modi) res))
        (let [[a b c] (first prs)
              val-in-res (get res b 2)
              new-val (+ val-in-res c)]
          (if (== c 1)
            (recur
             (assoc res b val-in-res)
             (insert-by first
                        [(*' (logbase 2 b) 2) b 2]
                        (rest prs))
             (+ state 1))
            (recur
             (assoc res b new-val)
             (insert-by first
                        [(*' new-val (logbase 2 b)) b new-val]
                        (rest prs))
             (+ state 1))))))))

(defn insert-by
  [f x xs]
  (concat (take-while #(<= (f %) (f x)) xs)
          [x]
          (drop-while #(<= (f %) (f x)) xs)))

(defn sol500b
  [target lim]
  (let [modi 500500507
        primes (map #(vector (logbase 2 %) % 1) (sieve lim))
        res (int-array (+ lim 1) 0)]
    (loop [prs primes state (long 0)]
      (if (>= state target)
        (reduce #(mod (*' %1 %2) modi)
                (map-indexed #(if (== %2 0) 1 (modex %1 (dec %2) modi))
                             (vec res)))
        (let [[a b c] (first prs)
              val-in-res (let [bres (aget res b)]
                           (if (== 0 bres) 2 bres))
              new-val (+ val-in-res c)]
          (if (== c 1)
            (recur
             (insert-by first
                        [(*' (logbase 2 b) 2) b 2]
                        (rest prs))
             (do (aset res b val-in-res)
                 (+ state 1)))
            (recur
             (insert-by first
                        [(*' new-val (logbase 2 b)) b new-val]
                        (rest prs))
             (do (aset res b new-val)
                 (+ state 1)))))))))

(defn sol500a
  [target lim]
  (let [modi 500500507
        primes (map #(vector (logbase 2 %) % 1) (sieve lim))
        res (int-array (+ lim 1) 0)]
    (loop [prs primes state (long 0)]
      (if (>= state target)
        (reduce #(mod (*' %1 %2) modi)
                (map-indexed #(if (== %2 0) 1 (modex %1 (dec %2) modi))
                             (vec res)))
        (let [[a b c] (first prs)
              val-in-res (let [bres (aget res b)]
                           (if (== 0 bres) 2 bres))
              new-val (+ val-in-res c)]
          (if (== c 1)
            (recur
             (insert-by first
                        [(*' (logbase 2 b) 2) b 2]
                        (rest prs))
             (do (aset res b val-in-res)
                 (+ state 1)))
            (recur
             (insert-by first
                        [(*' new-val (logbase 2 b)) b new-val]
                        (rest prs))
             (do (aset res b new-val)
                 (+ state 1)))))))))

(defn ^longs primes-range
  [^long a ^long b]
  (if (even? a)
    (filter prime? (range (+ a 1) (+ 1 b) 2))
    (filter prime? (range a (+ 1 b) 2))))

(defn sol500a
  [^long target]
  (let [modi 500500507]
    (loop [prs [4 9] lprime (int 5)
           limprime (int 0) state (long 2)]
      (if (>= state target)
        (->> (primes-range 3 limprime)
             (drop (- (count prs) 1))
             (concat (map #(int (Math/sqrt %))))
             (reduce #(mod (*' %1 %2) modi)))
        (let [[a b] prs psqr (* lprime lprime)]
          (cond (< psqr a) (let [tmp (primes-range psqr a)
                                 ctr (+ 1 (count tmp))
                                 sel (- target state)]
                             (cond (empty? tmp)
                                   (recur (cons psqr prs)
                                          (next-prime lprime)
                                          limprime
                                          (+ state 1))
                                   (> ctr sel)
                                   (recur (cons psqr prs)
                                          (next-prime lprime)
                                          (nth tmp (- sel 1))
                                          target)
                                   :else
                                   (recur (cons psqr prs)
                                          (next-prime lprime)
                                          limprime
                                          (+ state ctr))))
                (< psqr b) (let [tmp (primes-range a psqr)
                                 ctr (+ 1 (count tmp))
                                 sel (- target state)]
                             (cond (empty? tmp)
                                   (recur (insert psqr (insert (* a a) (rest prs)))
                                          (next-prime lprime)
                                          limprime
                                          (+ state 1))
                                   (> ctr sel)
                                   (recur (insert psqr (insert (* a a) (rest prs)))
                                          (next-prime lprime)
                                          (nth tmp (- sel 1))
                                          target)
                                   :else
                                   (recur (insert psqr (insert (* a a) (rest prs)))
                                          (next-prime lprime)
                                          limprime
                                          (+ state ctr))))
                :else (let [tmp (primes-range a b)
                            ctr (+ 1 (count tmp))
                            sel (- target state)]
                        (cond (empty? tmp)
                              (recur (insert (* a a) (rest prs))
                                     lprime
                                     limprime
                                     (+ state 1))
                              (> ctr sel)
                              (recur (insert (* a a) (rest prs))
                                     lprime
                                     (nth tmp (dec sel))
                                     target)
                              :else
                              (recur (insert (* a a) (rest prs))
                                     lprime
                                     (last tmp)
                                     (+ state ctr))))))))))

(defn solfor
  "This supposed to be the one that works"
  [^long target ^long lim]
  (let [primes (sieve lim)
        modi (long 500500507)
        sqr (fn [^long x] (*' x x))
        additions (->> (take 2000 primes)
                       (mapcat #(take-while
                                 (fn [x] (< x lim))
                                 (iterate sqr %)))
                       (concat primes)
                       sort
                       (take target))]
    (loop [i (int 0) [x & xs] additions res (long 0)]
      (if (> i target)
        res
        (recur (+ i 1) xs (rem (* res x) modi))))))

;; Runs in 218ms
(defn sol500
  "This supposed to be the one that is very very fast"
  [^long target ^long lim]
  (let [primes (long-array (take (+ 1 target) (sieve lim)))
        sqr (fn [^long x] (* x x))
        modi 500500507
        powers (->> (range (quot target 1000))
                    (map #(sqr (aget primes %)))
                    (take-while #(<= % lim))
                    (into []) 
                    (mapcat #(take-while (fn [x] (<= x lim))
                                         (iterate sqr %)))
                    sort int-array)]
    (loop [ctr (int 0) res (long 1) i (int 0) j (int 0)]
      (if (>= ctr target)
        res
        (let [pi (aget primes i)
              pj (aget powers j)]
          (if (< pi pj)
            (recur (+ ctr 1) (mod (* res pi) modi) (+ i 1) j)
            (recur (+ ctr 1) (mod (* res pj) modi) i (+ j 1))))))))

;; Runs in 197ms best time
(defn sol500f
  "This supposed to be the one that is very very fast"
  [^long target ^long lim]
  (let [primes (take (+ 1 target) (sieve lim))
        sqr (fn [^long x] (* x x))
        modi 500500507
        powers (->> (range target)
                    (map #(sqr (nth primes %)))
                    (take-while #(<= % lim))
                    (mapcat #(take-while (fn [x] (<= x lim))
                                         (iterate sqr %)))
                    sort)]
    (loop [ctr (int 0) res (long 1)
           is primes js powers]
      (if (>= ctr target)
        res
        (let [i (first is) j (first js)]
          (if (< i j)
            (recur (+ ctr 1) (mod (* res i) modi) (rest is) js)
            (recur (+ ctr 1) (mod (* res j) modi) is (rest js))))))))




