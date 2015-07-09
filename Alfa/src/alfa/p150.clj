(ns alfa.p150
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

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

(defn ^long sol136
  [^long lim]
  (let [refs (int-array lim 0)]
    (do (loop [a (int 0)]
          (if (>= a lim)
            nil
            (do (loop [b (quot a 4)]
                  (if (>= b (- a 1))
                    nil
                    (let [n (* a (- (* 4 b) a))]
                      (if (< 0 n lim)
                        (do (aset refs n (+ 1 (aget refs n)))
                            (recur (+ b 1)))
                        (if (>= n lim)
                          nil
                          (recur (+ b 1)))))))
                (recur (+ a 1)))))
        (loop [i (int 1) res (int 0)]
          (if (>= i lim)
            res
            (if (== 1 (aget refs i))
              (recur (+ i 1) (+ res 1))
              (recur (+ i 1) res)))))))

(defn ^long sol136d
  [^long lim]
  (let [refs (int-array lim 0)
        res (atom (int 0))]
    (do (doseq [a (range 1 lim)]
          (doseq [b (range (quot a 4) (- a 1))
                  :let [n (* a (- (* 4 b) a))]
                  :while (< n lim)
                  :when (< 0 n lim)]
            (aset refs n (+ 1 (aget refs n)))))
        (doseq [a (range 1 lim)]
          (when (== 1 (aget refs a))
            (swap! res inc)))
        @res)))

(defn abc-hits
  [lim]
  (let [res (atom (int 0))]
    (loop [a (int 1) ]
      (if (== a (- (quot lim 2) 1))
        @res
        (do (loop [b (int (+ 1 a))]
              (let [c (+ a b)]
                (if (> c lim)
                  nil
                  (if (== 1 (gcd a b) (gcd a c) (gcd b c))
                    (if (< (radical (* a b c)) c)
                      (do (swap! res inc)
                          (recur (+ b 1)))
                      (recur (+ b 1)))
                    (recur (+ b 1))))))
            (recur (+ a 1)))))))

(comment (defn repeating-decimal?
           "Returns -a if a/b is a non terminating decimal, and a if otherwise"
           [a b]
           (loop [n (int a) res []]
             (let [m (quot n b)
                   mi (rem n b)]
               (if (== mi 0)
                 (- a)
                 (if (some #(== mi %) res)
                   a
                   (recur (* mi 10) (conj res mi))))))))

(defn sol162
  [lim]
  (let [euler (Math/exp 1)]
    (loop [m (int 5) res (int 0)]
      (if (> m lim)
        res
        (let [st (int (Math/round (/ m euler)))
              stm (/ m st)]
          (recur (+ m 1)
                 (+ res (repeating-decimal? m st))))))))

(defn memax
  [[x & xs :as lst]]
  (if xs
    (let [len (quot (count lst) 2)
          lmax (memax (take len lst))
          rmax (memax (drop len lst))]
      (if (> lmax rmax)
        lmax
        rmax))
    (if x x 0)))



(defn isort
  [[x & xs]]
  (if xs
    (let [sorted-xs (isort xs)]
      (concat (take-while #(<= % x) sorted-xs)
              [x]
              (drop-while #(<= % x) sorted-xs)))
    (if x [x] [])))

(def fibo-pinter
  (memoize
    (fn [n]
      (cond (== n 0) 1
            (== n 1) 1
            :else (+' (fibo-pinter (- n 1))
                      (fibo-pinter (- n 2)))))))

(def refs (atom {}))

(defn fibo-pinteran-dikit
  [n]
  (cond (== n 0) 1
        (== n 1) 1
        :else (if-let [memon (get @refs n)]
                memon
                (let [value (+' (fibo-pinteran-dikit (- n 1))
                                (fibo-pinteran-dikit (- n 2)))]
                  (do (swap! refs assoc n value)
                      value)))))

(defn ^long sol127
  [^long lim]
  (let [refs (long-array lim 1)
        prs (boolean-array lim true)
        llim (int (Math/ceil (Math/sqrt lim)))
        res (atom 0)]
    (do (doseq [i (range 2 llim)
                :when (aget prs i)]
          (doseq [j (range (* i i) lim i)]
            (aset prs j false)))
        (doseq [i (range 2 lim)
                :when (aget prs i)]
          (doseq [j (range i lim i)]
            (aset refs j (* i (aget refs j)))))
        (doseq [a (range 1 (- (quot lim 2) 1))]
          (doseq [b (range (+ a 1) lim)
                  :let [c (+ a b)]
                  :while (< c lim)
                  :when (and (== 1 (gcd a b))
                             (< (* (aget refs a)
                                   (aget refs b)
                                   (aget refs c))
                                c))]
            (swap! res + c)))
        @res)))

(defn sol139
  [lim]
  (let [refs (int-array lim 0)]
    (loop [m (int 2)]
      (if (> m (quot lim 2))
        (loop [i (int 1) res (int 0)]
          (if (>= i lim)
            res
            (recur (+ i 1) (+ res (aget refs i)))))
        (do (let [msqr (* m m)]
              (loop [n (int 1)]
                (if (>= n m)
                  nil
                  (let [nsqr (* n n)
                        a (- msqr nsqr)
                        b (* 2 m n)
                        c (+ msqr nsqr)
                        peri (+ a b c)]
                    (if (>= peri lim)
                      nil
                      (do (if (and (or (even? m) (even? n))
                                   (== 1 (gcd m n)))
                            (let [tmp1 (* 2 a b)
                                  tmp2 (int (Math/sqrt (- (* c c) tmp1)))]
                              (if (== 0 (rem c tmp2))
                                (loop [j (int peri)]
                                  (if (>= j lim)
                                    nil
                                    (do (aset refs j (+ 1 (aget refs j)))
                                        (recur (+ j peri))))))))
                          (recur (+ n 1))))))))
            (recur (+ m 1)))))))

(defn primes-to
  [lim]
  (let [refs (boolean-array (+ lim 1) true)
        root (int (Math/sqrt lim))]
    (do (doseq [i (range 2 lim)
                :while (<= i root)
                :when (aget refs i)]
          (doseq [j (range (* i i) lim i)]
            (aset refs j false)))
        (filter #(aget refs %) (range 2 lim)))))



(defn primes-to1
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (let [root (-> n Math/sqrt long),
        cmpsts (boolean-array (inc n)),
        cullp (fn [p]
                (loop [i (* p p)]
                  (if (<= i n)
                    (do (aset cmpsts i true)
                        (recur (+ i p))))))]
    (do (dorun (map #(cullp %) (filter #(not (aget cmpsts %))
                                       (range 2 (inc root)))))
        (filter #(not (aget cmpsts %)) (range 2 (inc n))))))

(defn primes-to2
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [max-prime]
  (let [sieve (fn [s n]
                (if (<= (* n n) max-prime)
                  (recur (if (s n)
                           (reduce #(assoc %1 %2 false) s (range (* n n) (inc max-prime) n))
                           s)
                         (inc n))
                  s))]
    (->> (-> (reduce conj (vector-of :boolean) (map #(= % %) (range (inc max-prime))))
             (assoc 0 false)
             (assoc 1 false)
             (sieve 2))
         (map-indexed #(vector %2 %1)) (filter first) (map second))))

(defn primes-tox
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (let [root (-> n Math/sqrt long),
        rootndx (long (/ (- root 3) 2)),
        ndx (long (/ (- n 3) 2)),
        cmpsts (long-array (inc (/ ndx 64))),
        isprm #(zero? (bit-and (aget cmpsts (bit-shift-right % 6))
                               (bit-shift-left 1 (bit-and % 63)))),
        cullp (fn [i]
                (let [p (long (+ i i 3))]
                  (loop [i (bit-shift-right (- (* p p) 3) 1)]
                    (if (<= i ndx)
                      (do (let [w (bit-shift-right i 6)]
                            (aset cmpsts w (bit-or (aget cmpsts w)
                                                   (bit-shift-left 1 (bit-and i 63)))))
                          (recur (+ i p))))))),
        cull (fn [] (loop [i 0] (if (<= i rootndx)
                                  (do (if (isprm i) (cullp i)) (recur (inc i))))))]
    (letfn [(nxtprm [i] (if (<= i ndx)
                          (cons (+ i i 3) (lazy-seq (nxtprm (loop [i (inc i)]
                                                              (if (or (> i ndx) (isprm i)) i
                                                                                           (recur (inc i)))))))))]
      (do (cull) (cons 2 (lazy-seq (nxtprm 0)))))))

(defn maxi
  [lst]
  (if (empty? lst)
    0
    (let [x (first lst)
          nmax (maxi (rest lst))]
      (if (> nmax x)
        nmax
        x))))

(defn maxi
  [[x & xs]]
  (if xs
    (let [nmax (maxi xs)]
      (if (> nmax x) nmax x))
    x))

(defn ^long repdiv
  [^long n]
  (if (== 1 (gcd n 10))
    (loop [x (int 1) k (int 1)]
      (if (== x 0)
        k
        (recur (rem (+ 1 (* 10 x)) n) (+ 1 k))))
    0))

(defn ^long sol130
  [^long lim]
  (loop [i (int 3) res (int 0) ctr (int 0)]
    (cond
      (== ctr lim) res
      (prime? i) (recur (+ i 2) res ctr)
      :else (let [an (repdiv i)]
              (if (== 0 an)
                (recur (+ i 2) res ctr)
                (if (== 0 (rem (- i 1) an))
                  (recur (+ i 2) (+ res i) (+ ctr 1))
                  (recur (+ i 2) res ctr)))))))

(defn ^long sol129
  [^long lim]
  (first (drop-while #(<= (repdiv %) lim) (iterate #(+ 2 %) 1001))))

(defn maxi
  [coll]
  (loop [[x & xs] coll res x]
    (if x
      (recur xs (if (> x res) x res))
      res)))

(defn pyta
  [b h]
  (let [a (/ b 2)
        asqr (*' a a)
        h1sqr (*' h h)
        l (+' asqr h1sqr)
        lsqrt (Math/sqrt l)]
    (if (== lsqrt (bigint lsqrt)) [(bigint lsqrt)] [])))


(defn tsol138
  [lim]
  (loop [b (int 2) res [] ctr 0]
    (if (>= ctr lim)
      res
      (let [pa (pyta b (-' b 1))
            pb (pyta b (+' b 1))
            cpab (concat pa pb)]
        (recur (+' b 1)
               (into [] (concat res cpab))
               (+ ctr (count cpab)))))))

(def bahan
  (let [mentah (->> (slurp "resources/p107.txt")
                    (cs/split-lines)
                    (map #(cs/replace % #"-" "nil"))
                    (map #(cs/split % #","))
                    (mapv #(mapv read-string %)))]
    (loop [i (int 0) [x & xs] mentah res {}]
      (if (== i 40)
        res
        (let [tmp (loop [j (+ i 1) [k & ks] x resi {}]
                    (if (== j 40)
                      resi
                      (if k
                        (recur (+ j 1) ks (assoc resi [i j] k))
                        (recur (+ j 1) ks resi))))]
          (recur (+ i 1) xs (merge res tmp)))))))



(defn checkone
  [[x & xs]]
  (reduce #(let [r (last (last %1))]
            (if (> (- %2 r) 1)
              (conj %1 [%2])
              (conj (vec (butlast %1))
                    (conj (last %1) %2)))) [[x]] xs))

(defn ^long digsquare
  [^long n]
  (loop [i (int n) res (int 0)]
    (if (< i 10)
      (+ res (* i i))
      (recur (quot i 10)
             (let [m (rem i 10)]
               (+ res (* m m)))))))

(defn ^long sol92
  [^long lim]
  (let [refs (int-array (+ lim 1) 0)]
    (loop [i (int 1) res (int 0)]
      (let [ndig (digsquare i)]
        (if (> ndig 0)
          nil
          (cond (== 1 ndig) (aset refs i 1)
                (== 89 ndig) (aset refs i 2)
                :else nil))))))

(defn ^long sdigsquare
  [^long n]
  (cond (== n 1) 0
        (== n 89) 1
        :else (sdigsquare (digsquare n))))

(defn ^long sol92a
  [^long lim]
  (loop [i (int 1) res (int 0)]
    (if (> i lim)
      res
      (recur (+ i 1) (+ res (sdigsquare i))))))

;; no 141

(defn split-divs
  "Split the divisors of n into three seqs"
  [^long n]
  (partition-by #(== (Math/sqrt n) %) (divisors n)))

(defn psqr-progressive
  "Returns the list of perfect progressives from a,b,c"
  [^long a ^long b ^long c]
  (filter psqr? [(+ a (* b c)) (+ b (* a c))]))

(defn sol141
  [^long lim]
  (let [llim (int (Math/sqrt lim))]
    (loop [i (int 1) res #{}]
      (if (> i lim)
        (reduce + res)
        (let [[a [b] c] (split-divs i)
              resi (sequence
                     (filter #(<= % lim))
                     (mapcat #(psqr-progressive %1 b %2) a c))]
          (recur (+ i 1) (union res resi)))))))










































