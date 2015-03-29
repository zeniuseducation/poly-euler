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






