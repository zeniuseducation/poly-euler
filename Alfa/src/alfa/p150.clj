(ns alfa.p150
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]))

(def fast-expt
  (fn [^long n]
    (cond
      (== n 1) #{1}
      :else
      (let [tmp (map #(union (fast-expt %)
                             (fast-expt (- n %)))
                     (range 1 (inc (quot n 2))))]
        (conj (min-by count tmp) n)))))

(defn ^longs diopa
  [^long n]
  (let [diop?
        (fn [i]
          (let [x (* n i)]
            (== (gcd x (- i 1)) (- i 1))))]
    (sequence
      (comp (take-while diop?)
            (map #(* % n)))
      (iterate inc 2))))

(declare diop)

(defn diop-lista
  [^long target]
  (loop [i (int 2)]
    (let [tmp (diop (fact i))
          ctr (count tmp)]
      (if (> ctr target)
        [i (fact i) tmp ctr]
        (recur (+ i 1))))))

(declare kv-product)


(def closest-diopa
  (memoize
    (fn [^long target factors ^long maxi]
      (let [tmp (kv-product factors)]
        (if (> tmp maxi)
          nil
          (let [candidates
                (->> factors
                     (map #(->> {(+ 2 (key %)) 1 (key %) -1}
                                (merge-with + factors)))
                     (cons (merge-with + factors {3 1})))]))))))

(defn minimal-diopa
  [^long target]
  (let [maxi (+ target (quot target 4))]
    (loop [res #{{3 1}} i (int 0)]
      (if (> i 10)
        (->> (map #(vector (kv-product %) %) res)
             (filter #(< target (first %)))
             (min-by first))
        (let [tmp (->> (map #(vector (kv-product %) %) res)
                       (filter #(< target (first %) maxi))
                       (map second))]
          (if (not-empty tmp)
            (recur (->> (for [r res]
                          (->> r
                               (map #(->> {(+ 2 (key %)) 1 (key %) -1}
                                          (merge-with + r)))
                               (cons (merge-with + r {3 1}))))
                        (apply concat)
                        (filter #(every? (fn [x] (pos? (val x))) %))
                        (concat tmp)
                        (set))
                   (+ i 1))
            (recur (->> (for [r res]
                          (->> r
                               (map #(->> {(+ 2 (key %)) 1 (key %) -1}
                                          (merge-with + r)))
                               (cons (merge-with + r {3 1}))))
                        (apply concat)
                        (filter #(every? (fn [x] (pos? (val x))) %))
                        (set))
                   i)))))))

(defn kv-product
  "Returns the products key-value products in a-map"
  [amap]
  (reduce *' (map #(expt (key %) (val %)) amap)))

(defn mini-diop
  [^long tar ^long loopi]
  (let [target (inc (* tar 2))
        maxi (+ target (quot target 4))
        primes (sieve 100)
        calc (fn [xs]
               (let [bahan (sort-by key > xs)]
                 (loop [[k & ks] bahan res (long 1) prs primes]
                   (if k
                     (recur ks
                            (->> (take (val k) prs)
                                 (map #(expt % (quot (dec (key k)) 2)))
                                 (reduce *')
                                 (*' res))
                            (drop (val k) prs))
                     res))))]
    (loop [res #{{3 1}} i (int 0)]
      (if (> i loopi)
        (->> (map #(vector (kv-product %) %) res)
             (filter #(< target (first %) maxi))
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
                    (concat res)
                    set)
               (+ i 1))))))

(defn fpoli
  [^longs xs]
  (fn [n] (reduce + (map-indexed #(* %2 (expt n %1)) xs))))

(defn last-pow
  [^long a ^long b]
  (last (take-while #(<= (expt b %) a) (range))))

(defn sol75a
  [^long lim]
  (let [refs (int-array (+ lim 1) 0)]
    (do (doseq [m (range 2 (+ 1 (quot lim 2)))
                :let [msqr (* m m)]]
          (doseq [n (range 1 m)
                  :let [nsqr (* n n)
                        a (- msqr nsqr)
                        b (* 2 m n)
                        c (+ msqr nsqr)
                        peri (+ a b c)]
                  :when (and (or (even? m) (even? n))
                             (== 1 (gcd m n)))
                  :while (<= peri lim)]
            (doseq [i (range peri (+ 1 lim) peri)]
              (aset refs i (+ 1 (aget refs i))))))
        (count (filter #(== 1 (aget refs %)) (range 12 (+ lim 1) 2))))))

(defn sol75
  [^long lim]
  (let [refs (int-array (+ lim 1) 0)
        llim (quot lim 2)]
    (do (loop [m (int 2)]
          (if (> m llim)
            nil
            (let [msqr (long (* m m))]
              (do (loop [n (int 1)]
                    (if (== m n)
                      nil
                      (let [nsqr (long (* n n))
                            a (- msqr nsqr)
                            b (* 2 m n)
                            c (+ msqr nsqr)
                            peri (+ a b c)]
                        (if (> peri lim)
                          nil
                          (if (and (== 1 (gcd m n))
                                   (or (even? m) (even? n)))
                            (do (loop [pp (int peri)]
                                  (if (> pp lim)
                                    nil
                                    (do (aset refs pp (+ 1 (aget refs pp)))
                                        (recur (+ peri pp)))))
                                (recur (+ n 1)))
                            (recur (+ n 1)))))))
                  (recur (+ m 1))))))
        (loop [k (int 12) ctr (int 0)]
          (if (> k lim)
            ctr
            (if (== 1 (aget refs k))
              (recur (+ k 2) (+ ctr 1))
              (recur (+ k 2) ctr)))))))






