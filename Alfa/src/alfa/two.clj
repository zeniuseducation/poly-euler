(ns alfa.two
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all]))

(def cm (set! *unchecked-math* true))

(defn sol491
  "Solution for number 491"
  [^long lim]
  (let [idem (fn [^long x]
               (loop [i (int (- x 1))]
                 (if (== i (rem (* i i) x))
                   i
                   (recur (- i 1)))))]
    (->> (pmap idem (range 1 (+ 1 lim)))
         (reduce +))))

(defm collatz
  [^long n]
  (cond
   (== 1 n) 1
   (== 0 (rem n 2)) (+ 1 (collatz (quot n 2)))
   :else (+ 1 (collatz (+ 1 (* 3 n))))))

(defn ^long solp
  "no 14"
  [^long lim]
  (max-by second
          (sequence
           (map #(vector % (collatz %)))
           (range 1 lim))))

(defn ^long soln
  "no 14 normal"
  [^long lim]
  (loop [i (int 1) res (int 0) resi (int 1)]
    (if (> i lim)
      resi
      (let [tmp (collatz i)]
        (if (> tmp res)
          (recur (+ i 1) tmp i)
          (recur (+ i 1) res resi))))))

(defn sol
  [^long lim]
  (let [refs (int-array (+ 1 lim) 1)
        divs (fn [^long n]
               (if (even? n)
                 (loop [i (int 2) res [1 n]]
                   (if (>= (* i i) n)
                     (if (== (* i i) n)
                       (vec (sort (conj res i)))
                       (vec (sort res)))
                     (recur (+ i 1)
                            (if (== 0 (rem n i))
                              (conj res i (quot n i))
                              res))))
                 (loop [i (int 3) res [1 n]]
                   (if (>= (* i i) n)
                     (if (== (* i i) n)
                       (vec (sort (conj res i)))
                       (vec (sort res)))
                     (recur (+ i 2)
                            (if (== 0 (rem n i))
                              (conj res i (quot n i))
                              res))))))]
    (loop [i (long 1)]
      (if (> i lim)
        (->> (pmap #(aget refs %) (range 1 (+ 1 lim)))
             (remove #{1})
             (sort))
        (do (let [factors (divs (* i (- i 1)))]
              (doseq [d (take-while #(<= % lim) factors)]
                (when (< i d)
                  (aset refs d i))))
            (recur (+ i 1)))))))

(defn ^boolean odd-prime?
  [^long n]
  (loop [i (int 3)]
    (cond (> (* i i) n) true
          (== 0 (rem n i)) false
          :else (recur (+ i 2)))))

(defn ^long sol293
  [^long tar]
  (let [lim (+ tar 1000)
        refs (boolean-array lim true)
        llim (int (Math/sqrt lim))
        bahan [2 3 5 7 11 13 17 19 23]
        init (loop [i (int 3)]
               (if (> i llim)
                 nil
                 (if (aget refs i)
                   (do (loop [j (int (* i i))]
                         (if (> j lim)
                           nil
                           (recur (do (aset refs j false)
                                      (+ j (* 2 i))))))
                       (recur (+ i 2)))
                   (recur (+ i 2)))))
        mul2 (->> (iterate inc 1)
                  (map #(expt 2 %))
                  (take-while #(< % lim)))
        nprime (fn [^long x]
                 (- (->> (iterate #(+ 2 %) (+ x 3))
                         (drop-while #(not (aget refs %)))
                         first) x))
        insert (fn [xs e]
                 (concat (take-while #(< % e) xs)
                         [e]
                         (drop-while #(< % e) xs)))
        kums (atom (apply sorted-set (reductions * bahan)))]
    (loop [i (int 0)]
      (if (> i 8)
        (reduce + (distinct (pmap nprime (take-while #(<= % lim) @kums))))
        (do (loop [res [(reduce * (take (inc i) bahan))]]
              (let [tmp (->> (for [r res b (take (inc i) bahan)
                                   :let [rb (* r b)]
                                   :when (<= rb lim)]
                               rb)
                             sort)]
                (if (empty? tmp)
                  nil
                  (do (reset! kums (union @kums (set tmp)))
                      (recur tmp)))))
            (recur (+ i 1)))))))

(defn ^longs check-sieve
  [^long lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))]
    (loop [i (int 3) m (int 2) res (sorted-set 1)]
      (if (> i lim)
        (reduce + res)
        (if (aget refs i)
          (do (when (<= i llim)
                (loop [j (int (* i i))]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j (* 2 i)))))))
              (recur (+ i 2) (- i 1) (conj res (- i m))))
          (recur (+ i 2) m res))))))

(defn ^long sol
  [^long lim]
  (let [refs (sieve (inc (int (Math/sqrt lim))))
        prime? (fn [^long p]
                 (every? #(not= 0 (rem p %))
                         (take-while #(<= (* % %) p) refs)))
        nprime (fn [^long x]
                 (loop [i (long (+ x 3))]
                   (if (prime? i)
                     (- i x)
                     (recur (+ i 2)))))
        bahan [2 3 5 7 11 13 17 19 23]
        admis? (fn [^long n]
                 (loop [p (long n) i (int 0)]
                   (cond
                    (== 1 p) true
                    (== 9 i) false
                    :else (let [tmp (bahan i)
                                jmp (->> (iterate #(/ % tmp) p)
                                         (take-while integer?)
                                         last)]
                            (if (== jmp p)
                              false
                              (recur jmp (+ i 1)))))))]
    (->> (sequence
          (comp (filter admis?)
                (map nprime)) 
          (range 2 (+ lim 1) 2))
         distinct
         (reduce +))))

(defn sol
  [^long lim]
  (let [term (fn [x]
               (loop [i (int (- x 1)) res 0]
                 (let [tmp (fexpt (/ x i) i)]
                   (if (> tmp res)
                     (recur (- i 1) tmp)
                     (try (bigdec res)
                          (- x)
                          (catch ArithmeticException e x)
                          (finally x))))))]
    (->> (range 5 (+ 1 lim))
         (pmap term)
         (reduce +))))

(def digs (->> (range 10)
               (mapcat #(list [(str % "a") %]
                              [(str % "b") %]))
               (into {})))

(def sdigs (set (keys digs)))

(def mbob (combine sdigs 10))

(defn bcolnum
  [ls]
  (loop [[x & xs] ls res 0]
    (if x
      (recur xs (+ (*' 10 res) x))
      res)))

(defn div11
  []
  (let [div? (fn [st]
               (let [anti (difference sdigs st)
                     sst (apply + (map #(get digs %) st))
                     santi (apply + (map #(get digs %) anti))]
                 (== 0 (rem (- sst santi) 11))))
        pasang (fn [st]
                 (let [anti (difference sdigs st)
                       sst (map #(get digs %) st)
                       stzero (count (filter #{0} sst))
                       santi (map #(get digs %) anti)
                       dst (count (distinct sst))
                       dsanti (count (distinct santi))
                       fact9 (fact 9)]
                   [(sort sst)
                    (* (quot (* (- 10 stzero) fact9)
                             (apply * (repeat (- 10 dst) 2)))
                       (quot (* 10 fact9)
                             (apply * (repeat (- 10 dsanti) 2))))]))]
    (->> (filter div? mbob)
         (map pasang)
         (group-by first)
         vals
         (map #(second (first %)))
         (reduce +'))))

(defn unitary
  [n]
  (let [num (fact n)
        unit? (fn [x]
                (== 1 (gcd x (quot num x))))]
    (filter unit? (divisors num))))

(defn parts
  [^long n]
  (loop [i (int 2) res (map vector (range 2 (+ 1 n))) tres (int 9999999)]
    (if (or (> i 12) (> i n) (empty? res))
      tres
      (let [tmp (for [r res
                      :let [lr (last r)]
                      m (iterate inc lr)
                      :let [mr (conj r m)]
                      :while (<= (apply * mr)
                                 (+ (- n i) (apply + mr)))] mr) 
            jmp (-> #(== (+ (- n i) (apply + %))
                         (apply * %))
                    (filter tmp))]
        (recur (+ i 1)
               tmp
               (if (empty? jmp)
                 tres
                 (let [smp (map #(apply * %) jmp)
                       min-smp (apply min smp)]
                   (if (< min-smp tres) min-smp tres))))))))

(defn ^long sol
  [^long lim]
  (->> (range 2 (+ 1 lim))
       (pmap parts)
       set
       (reduce +)))

(defn crazy
  [^long lim ^long modi]
  (let [llim (long (Math/sqrt lim))
        ssqr (fn [n]
               (rem (quot (*' n (+ 1 n) (+ 1 (* 2 n))) 6) modi))
        sum (fn [n]
              (let [limsum (quot lim n)]
                (rem (+ (- (ssqr limsum)
                           (ssqr n))
                        (*' (*' n n) (- limsum (- n 1))))
                     modi)))]
    (loop [i (long 1) res 0]
      (if (> i llim)
        res
        (recur (+ 1 i)
               (rem (+ res (sum i)) modi))))))

(defn sol
  [^long lim ^long modi]
  (let [llim (long (Math/sqrt lim))
        ssqr (fn [n]
               (rem (quot (*' n (+ 1 n) (+ 1 (* 2 n))) 6) modi))
        sum (fn [n]
              (let [limsum (quot lim n)]
                (rem (+ (- (ssqr limsum)
                           (ssqr n))
                        (*' (*' n n) (- limsum (- n 1))))
                     modi)))]
    (->> (range 1 (+ 1 llim))
         (pmap sum)
         (reduce #(rem (+ %1 %2) modi)))))

(defn ^boolean psqr?
  [^long n]
  (let [ps? (fn [x]
              (let [xs (Math/sqrt x)]
                (== (bigint xs) xs)))]
    (if (even? n)
      (loop [i (long 2) res (bigint (+ 1 (*' n n)))]
        (if (>= (*' i i) n)
          (if (== (*' i i) n)
            (ps? (+ res (*' i i)))
            (ps? res))
          (recur (+ i 1)
                 (if (== 0 (rem n i))
                   (+ res (*' i i) (let [t (quot n i)] (*' t t)))
                   res))))
      (loop [i (long 3) res (bigint (+ 1 (*' n n)))]
        (if (>= (* i i) n)
          (if (== (*' i i) n)
            (ps? (+ res (*' i i)))
            (ps? res))
          (recur (+ i 2)
                 (if (== 0 (rem n i))
                   (+ res (*' i i) (let [t (quot n i)] (*' t t)))
                   res)))))))

(defn sol
  [^long lim]
  (loop [i (long 1) res (bigint 0)]
    (if (== i lim)
      res
      (recur (+ i 1)
             (if (psqr? i)
               (+' res i)
               res)))))

(defn sola
  [^long lim]
  (transduce
   (filter psqr?)
   +' (range 1 lim)))

(def counting
  (memoize
   (fn [^long m ^long n ^long i]
     (if (= i 700)
       (+ 1 (- 9 (+ m n)))
       (let [mk (- 9 (+ m n))]
         (reduce +' (map #(counting n % (+ 1 i))
                         (range 0 (+ 1 mk)))))))))

(defn sol
  []
  (->> (counting i j 1)
       (for [i (range 1 10)
             j (range 10)
             :when (<= (+ i j) 9)])
       (reduce +')))



(defn search
  [lim]
  (let [tores (atom [])
        counting (fn [xs]
                   (let [xsum (->> (map #(let [x (/ 1 %)]
                                           (* x x)) xs)
                                   (reduce +))]
                     (do (when (>= 1/2 xsum)
                           (swap! tores conj xsum))
                         xsum)))]
    (loop [i (int 2) res [[]]]
      (if (> i lim)
        [@tores (count res)]
        (let [xs (map #(conj % i) res)
              tres (filter #(< (counting %) 1/2) xs)]
          (recur (+ i 1)
                 (concat res tres)))))))

(defn parts
  [raw cur]
  (let [scur (apply + cur)]
    (cond
     (> scur 1/2) 0
     (== scur 1/2) 1
     (empty? raw) 0
     :else (->> raw
                (map #(parts (last (partition-by #{%} raw))
                             (conj cur %)))
                (apply +)))))

(defn ^long pascali
  [^long n ^long lim]
  (loop [i (long 0) res [0] cur [[1]]]
    (if (> i lim)
      res 
      (let [nexi (cons 1 (conj (mapv #(rem (+ %1 %2) n)
                                     (butlast cur)
                                     (rest cur)) 1))
            suma (-> #(== 0 (rem % n))
                     (filter nexi)
                     count)]
        (recur (+ 1 i)
               (if (== 0 (rem (+ i 1) n))
                 (conj res suma)
                 (conj (vec (butlast res))
                       (+ (last res) suma)))
               nexi)))))

(defn triad
  [n]
  (quot (*' n (- n 1)) 2))

(def pascal
  (memoize
   (fn [base n]
     (cond
      (= n 0) 0
      :else (+' (*' (triad (+ 1 base)) (pascal base (- n 1)))
                (*' (triad base) (triad (expt base (- n 1)))))))))

(defm main-side
  [base n]
  (triad (expt base n)))

(defm fsol
  [base n]
  (if (<= n base)
    0
    (let [ln (->> (range)
                  (drop-while #(zero? (quot (expt base %) n)))
                  first dec)
          exp (expt base ln)
          ctr (quot n exp)]
      (+ (*' (triad (+' 1 ctr)) (pascal base ln))
         (*' (triad ctr) (main-side base ln))
         (*' ctr (- (main-side base ln)
                   (triad (- exp (rem n exp)))))
         (*' (+ 1 ctr) (fsol base (rem n exp)))))))

(defm pascal-row
  [^long n ^long base]
  (if (<= n base)
    0
    (let [ln (->> (range)
                  (drop-while #(zero? (quot (expt base %) n)))
                  first dec)
          exp (expt base ln)
          ctr (quot n exp)
          cure (* ctr (- (- exp 1) (- n 1 (* ctr exp))))]
      (if (== 0 (rem n exp))
        0
        (+ cure
           (* (+ ctr 1)
              (pascal-row (quot (- n cure) (+ 1 ctr))
                          base)))))))

(defn sol
  [^long base ^long lim]
  (loop [i (long 1) res (long 0)]
    (if (> i lim)
      (- (triad lim) res)
      (recur (+ 1 i)
             (+ res (pascal-row i base))))))

(defn solp
  [^long base ^long lim]
  (- (triad lim)
     (fsol lim base)))

(defn tsol
  [lim]
  (-' (triad (+' 1N lim)) (fsol 7N lim)))


(defn primes
  [lim]
  (let [refs (boolean-array (+ lim 1) true)
        llim (int (Math/sqrt lim))]
    (loop [i (int 3) res (transient [])]
      (if (> i lim)
        (persistent! res)
        (if (aget refs i)
          (recur (+ i 2)
                 (do (when (<= i llim)
                       (loop [j (int (* i i))]
                         (recur (do (aset refs i false)
                                    (+ j (* i 2))))))
                     (conj! res i)))
          (recur (+ i 2) res))))))


