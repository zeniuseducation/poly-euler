(ns alfa.one
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]))

(def cm (set! *unchecked-math* true))

(defn ^long sol1a
  [^long lim]
  (loop [i (int 3) res (int 0)]
    (if (== i lim)
      res
      (recur (+ 1 i)
             (if (or (== 0 (rem i 3))
                     (== 0 (rem i 5)))
               (+ res i)
               res)))))

;; 0.25 msecs

(defn ^long sol1b
  [^long lim]
  (- (+ (apply + (range 3 lim 3))
        (apply + (range 5 lim 5)))
     (apply + (range 15 lim 15))))


;; 0.11 msec

(defn ^long sol1c
  [^long lim]
  (let [[a b c] (pvalues (apply + (range 3 lim 3))
                         (apply + (range 5 lim 5))
                         (apply + (range 15 lim 15)))]
    (- (+ a b) c)))

;; 0.28msecs

(defn ^long sol2a
  [^long lim]
  (loop [a (int 2) b (int 1) res (int 0)]
    (if (> a lim)
      res
      (recur (+ a b)
             a
             (if (even? a)
               (+ res a)
               res)))))

(defn odd-prime?
  [^long p]
  (loop [i (int 3)]
    (if (> (* i i) p)
      true
      (if (== 0 (rem p i))
        false
        (recur (+ 2 i))))))

(defn ^long sol3
  [^long n]
  (loop [p (long n)]
    (if (odd-prime? p)
      p
      (recur (loop [j (long 3)]
               (if (odd-prime? j)
                 (if (== 0 (rem p j))
                   (quot p j)
                   (recur (+ j 2)))
                 (recur (+ j 2))))))))

(defn ^long nth-sieve
  [^long tar]
  (let [lim (* tar 13)
        refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))]
    (loop [i (int 3) c (int 1)]
      (if (== c tar)
        (- i 2)
        (if (aget refs i)
          (do (when (<= i llim)
                (loop [j (* i i)]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j (* 2 i)))))))
              (recur (+ i 2) (+ c 1)))
          (recur (+ i 2) c))))))

(defn ^long cprimes
  [^long lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))
        cprime?
        (fn [n]
          (let [strn (str n)
                cnt (count strn)]
            (if (some (set (str "02468")) strn) false
                                                (loop [i 0]
                                                  (if (== i cnt)
                                                    true
                                                    (let [front (take i strn)
                                                          back (drop i strn)
                                                          hsl (concat back front)
                                                          res (->> (apply str hsl)
                                                                   read-string int)]
                                                      (if (aget refs res)
                                                        (recur (+ 1 i))
                                                        false)))))))]
    (do (loop [i (int 3)]
          (when (<= i lim)
            (if (aget refs i)
              (do (when (<= i llim)
                    (loop [j (* i i)]
                      (if (> j lim)
                        nil
                        (do (aset refs j false)
                            (recur (+ j (* 2 i)))))))
                  (recur (+ i 2)))
              (recur (+ i 2)))))
        (loop [i (int 3) res 1]
          (if (> i lim)
            res
            (recur (+ 2 i)
                   (if (cprime? i)
                     (+ res 1)
                     res)))))))

(defn ^long cprimes
  [^long lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))
        cprime?
        ;; This one checking circular-primality
        (fn [n]
          (let [strn (str n)
                cnt (int (count strn))]
            (if (some #{\0 \2 \4 \5 \6 \8} strn)
              false
              (->> strn
                   (iterate #(apply str (cons (last %) (butlast %))))
                   (take cnt) rest
                   (every? #(aget refs (int (read-string %))))))))]
    (loop [i (int 3) res (int 2)]
      (if (> i lim)
        res
        (if (aget refs i)
          (do (when (<= i llim)
                (loop [j (* i i)]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j (* 2 i)))))))
              (recur (+ i 2)
                     (if (cprime? i)
                       (+ res 1)
                       res)))
          (recur (+ i 2) res))))))

(defn ^long sum-sieve
  [^long lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))]
    (loop [i (int 3) res (int 2)]
      (if (> i lim)
        res
        (if (aget refs i)
          (do (when (<= i llim)
                (loop [j (int (* i i))]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j (* 2 i)))))))
              (recur (+ i 2) (+ res i)))
          (recur (+ i 2) res))))))

;; runs in 6.7ms

(defn ^long fsum-sieve
  [^long lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))
        n (quot (- lim 2) 2)]
    (loop [i (int 3) res (+ (quot (* n (+ 3 (- lim 1))) 2) 2)]
      (if (> i llim)
        res
        (if (aget refs i)
          (let [rst (loop [j (int (* i i)) resj res]
                      (if (> j lim)
                        resj
                        (if (aget refs j)
                          (do (aset refs j false)
                              (recur (+ j (* i 2))
                                     (- resj j)))
                          (recur (+ j (* i 2))
                                 resj))))]
            (recur (+ i 2) rst))
          (recur (+ i 2) res))))))

(def cdiv
  (memoize
    (fn [^long x]
      (if (== 0 (rem x 2))
        (loop [i (int 2) res (int 2)]
          (if (>= (* i i) x)
            (if (> (* i i) x) res (+ 1 res))
            (recur (+ i 1)
                   (if (== 0 (rem x i))
                     (+ res 2)
                     res))))
        (loop [i (int 3) res (int 2)]
          (if (>= (* i i) x)
            (if (> (* i i) x) res (+ 1 res))
            (recur (+ i 2)
                   (if (== 0 (rem x i))
                     (+ res 2)
                     res))))))))

(defn ^long sol12
  [^long tar]
  (loop [i (int 10)]
    (let [fact (if (== 0 (rem i 2))
                 (* (cdiv (quot i 2))
                    (cdiv (+ i 1)))
                 (* (cdiv (quot (+ i 1) 2))
                    (cdiv i)))]
      (if (> fact tar) (quot (* i (+ i 1)) 2) (recur (+ i 1))))))

(defm max-palin
      [^long i]
      (loop [n (int i)]
        (let [pal (int (+ (* 1000 n)
                          (->> (str n)
                               reverse
                               (apply str)
                               bigint)))]
          (if (some #(== 0 (rem pal %)) (range 999 101 -1))
            (let [jerad (->> (range 999 101 -1)
                             (drop-while #(not= 0 (rem pal %)))
                             first)
                  jared (quot pal jerad)]
              (if (or (> jared 999) (== jerad jared))
                (recur (- n 1))
                pal))
            (recur (- n 1))))))

(def nums
  (->> (slurp "resources/p8.txt")
       split-lines
       (apply concat)
       (map #(int (read-string (str %))))))

(defn ^long sol8
  [^longs bhn]
  (->> (iterate rest bhn)
       (map #(take 13 %))
       (take 987)
       (map #(apply * %))
       (apply max)))

(defm sumproper
      [^long x]
      (if (== 0 (rem x 2))
        (loop [i (int 2) res (int 1)]
          (if (>= (* i i) x)
            (if (> (* i i) x)
              res
              (+ i res))
            (recur (+ i 1)
                   (if (== 0 (rem x i))
                     (+ res i (quot x i))
                     res))))
        (loop [i (int 3) res (int 1)]
          (if (>= (* i i) x)
            (if (> (* i i) x)
              res
              (+ i res))
            (recur (+ i 2)
                   (if (== 0 (rem x i))
                     (+ res i (quot x i))
                     res))))))

(defn ^long amicables
  [^long lim]
  (loop [i (int 6) res (int 0)]
    (if (> i lim)
      res
      (recur (+ i 1)
             (let [sp (sumproper i)]
               (if (and (not= sp i) (== i (sumproper sp)))
                 (+ res i)
                 res))))))

(defm  psumprop
       [^long x]
       (if (== 0 (rem x 2))
         (->> (range 2 (Math/sqrt x))
              (keep #(when (== 0 (rem x %))
                      (+ % (quot x %))))
              (reduce + 1))
         (->> (range 3 (Math/sqrt x) 2)
              (keep #(when (== 0 (rem x %))
                      (+ % (quot x %))))
              (reduce + 1))))

(defn ^long amics
  [^long lim]
  (let [amic? (fn [i]
                (let [sp (sumproper i)]
                  (and (not= sp i)
                       (== i (sumproper sp)))))]
    (->> (range 6 lim)
         (r/filter amic? )
         (r/reduce +))))

(defn ^long cprimes
  [^long dig]
  (let [bhn [1 3 7 9]
        cprime? (fn [xs]
                  (let [cnt (count xs)]
                    (->> (cycle xs)
                         (iterate rest)
                         (map #(take cnt %))
                         (take cnt)
                         (every? #(odd-prime? (colnum %))))))]
    (->> (range 2 (+ 1 dig))
         (map #(->> (permutes bhn %)
                    (filter cprime?)
                    count))
         (r/fold +)
         (+ 4))))





(defn sol48
  [lim modulo]
  (rem (->> (range 1 lim)
            (pmap #(modex % % modulo))
            (reduce +'))
       modulo))

(defn sol206
  [^long n]
  (let [tes (map #(rem % 10) (range 1 11))]
    (loop [m (int 2) r [[0]]]
      (if (> m n)
        (map colnum (filter #(or (= (dec n) (count %))
                                 (= n (count %))) r))
        (let [check (take-last m tes)
              num (for [xs r
                        i (range 20)
                        :let [ls (cons i xs)
                              x (colnum ls)
                              xsq (*' x x)
                              rt (rem xsq (expt 10 (- (* 2 m) 1)))]
                        :when (= check
                                 (take-nth 2 (numcol rt)))]
                    (take-last (- (* 2 m) 1) (numcol x)))]
          (recur (+ m 1) (distinct num)))))))



(defn sol206b
  [st dig]
  (let [bahan (take-last dig [1 2 3 4 5 6 7 8 9 0])]
    (first (drop-while #(->> (rem (*' % %) (expt 10 (dec (* 2 dig))))
                             numcol
                             (take-nth 2)
                             (not= bahan))
                       (iterate #(+ 10 %) st)))))

(defn sol206c
  []
  (let [bahan (permutes (range 10) 5)
        check [9 1 7 0]]
    (-> #(let [num (colnum (concat [1] % check))]
          (not= (take-nth 2 (numcol (* num num)))
                [1 2 3 4 5 6 7 8 9 0]))
        (drop-while  bahan)
        first
        (concat check)
        colnum)))

(defn colbase2
  [^longs xs]
  (loop [[i & is] xs res (int 0)]
    (if i (recur is (+ (* 2 res) i)) res)))

(defn ^boolean all-diff?
  [^longs xs ^long n kk]
  (if (= kk :not)
    (->> (iterate rest xs)
         (map #(take n %))
         (take (- (count xs) (- n 1)))
         (apply distinct?))
    (->> (cycle xs)
         (iterate rest)
         (map #(take n %))
         (take (count xs))
         (apply distinct?))))

(defn ^long smallest-bin
  [^longs xs ^long len ^long n kk]
  (if (= kk :last)
    (->> (cycle xs)
         (iterate rest)
         (take len)
         (map #(take len %))
         (min-by colbase2))
    (->> (cycle xs)
         (iterate rest)
         (take (- len (- n 1)))
         (map #(take len %))
         (min-by colbase2))))

(defn ^longs sol265
  [^long n]
  (let [len (int (expt 2 n))]
    (loop [i (int n) res (permutes [0 1] n)]
      (if (== i len)
        (reduce +
                (-> (comp (filter #(all-diff? % n :last))
                          (map #(colbase2 (smallest-bin % len n :last))))
                    (eduction res)
                    distinct))
        (recur (+ i 1)
               (distinct
                 (eduction
                   (filter #(all-diff? % n :not))
                   (for [r res m [0 1]]
                     (conj r m)))))))))

(defn divs
  [^long x]
  (if (== 0 (rem x 2))
    (loop [i (int 2) res (transient [])]
      (if (>= (* i i) x)
        (if (> (* i i) x)
          (persistent! res)
          (persistent! (conj! res i)))
        (recur (+ i 1)
               (if (== 0 (rem x i))
                 (do (conj! res i)
                     (conj! res (quot x i)))
                 res))))
    (loop [i (int 3) res (transient [])]
      (if (>= (* i i) x)
        (if (> (* i i) x)
          (persistent! res)
          (persistent! (conj! res i)))
        (recur (+ i 2)
               (if (== 0 (rem x i))
                 (do (conj! res i )
                     (conj! res (quot x i)))
                 res))))))

(defn ^long sol351
  [^long lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (+ 1 (int (Math/sqrt lim)))
        pfactors (fn [n]
                   (loop [p (int n) i (int 3) r (transient #{})]
                     (if (and (not= p 2) (even? p))
                       (recur (quot p 2)
                              3
                              (conj! r 2))
                       (if (aget refs p)
                         (if (== (* p p) n)
                           (persistent! r)
                           (persistent! (conj! r p)))
                         (if (== 0 (rem p i))
                           (recur (quot p i)
                                  3
                                  (conj! r i))
                           (recur p (->> (iterate #(+ 2 %) (+ 2 i))
                                         (drop-while #(not (aget refs %)))
                                         first) r))))))
        totient (fn [i]
                  (let [pts (pfactors i)]
                    (quot (* i (reduce * (map dec pts)))
                          (reduce * pts))))]
    (do (loop [i (int 2) res (transient [])]
          (if (> i lim)
            (persistent! res)
            (if (aget refs i)
              (recur (+ i 1)
                     (do (when (<= i llim)
                           (loop [j (int (* i i))]
                             (if (> j lim)
                               nil
                               (do (aset refs j false)
                                   (recur (+ j i))))))
                         (conj! res i)))
              (recur (+ i 1) res))))
        (->> (range 2 (+ 1 lim))
             (pmap #(if (aget refs %)
                     6
                     (* 6 (- % (totient %)))))
             (reduce +)))))

(defn ^long sumdig
  [^long n]
  (loop [i (int n) res (int 0)]
    (if (< i 10)
      (+ res i)
      (recur (quot i 10) (+ res (rem i 10))))))

(defn ^long sol315
  [^long start ^long  end]
  (let [prefs (boolean-array (+ end 1) true)
        refs (transient [[1 1 1 1 1 0 1]
                         [0 0 1 1 0 0 0]
                         [0 1 1 0 1 1 1]
                         [0 0 1 1 1 1 1]
                         [1 0 1 1 0 1 0]
                         [1 0 0 1 1 1 1]
                         [1 1 0 1 1 1 1]
                         [1 0 1 1 1 0 0]
                         [1 1 1 1 1 1 1]
                         [1 0 1 1 1 1 1]])

        dig (fn [^long n]
              (loop [i (int n) res (transient [])]
                (if (< i 10)
                  (persistent! (conj! res i))
                  (let [d (rem i 10)]
                    (recur (quot i 10)
                           (conj! res d))))))

        trans (fn [xs1 xs2]
                (let [nc1 (map refs xs1)
                      nc2 (map refs xs2)]
                  (loop [[l1 & ls1] nc1 [l2 & ls2] nc2 res (int 0)]
                    (if l2
                      (recur ls1 ls2
                             (+ res (apply + (map #(rem (+ %1 %2) 2) l1 l2))))
                      (+ res (->> (cons l1 ls1)
                                  (apply concat)
                                  (reduce +)))))))

        diff (fn [^long n]
               (loop [i (int n) idig (dig i)
                      sam (int 0) max (->> (map #(apply + (refs %)) idig)
                                           (apply +)
                                           int)]
                 (if (< i 10)
                   (let [x (apply + (refs i))]
                     (- (+ sam (* 2 x)) (+ max x)))
                   (let [nexi (apply + idig)
                         nexcol (dig nexi)]
                     (recur nexi
                            nexcol
                            (+ sam (* 2 (->> (map #(apply + (refs %)) idig)
                                             (apply +))))
                            (+ max (trans idig nexcol)))))))
        llim (int (Math/sqrt end))]
    (loop [i (int 3) res (long 0)]
      (if (> i end)
        res
        (if (aget prefs i)
          (recur (+ i 2)
                 (do (when (<= i llim)
                       (loop [j (int (* i i))]
                         (if (> j end)
                           nil
                           (do (aset prefs j false)
                               (recur (+ j (* 2 i)))))))
                     (if (> i start)
                       (+ res (diff i))
                       res)))
          (recur (+ i 2) res))))))

(def prime100 (sieve 100))

(defn cham
  [^long n ^long p ^long lim]
  (let [np (int (* n p))]
    (if (> np lim)
      0
      (+ 1 (transduce
             (comp (drop-while #(< % p))
                   (map #(cham np % lim)))
             + prime100)))))

(defn rcham
  [^long lim]
  (cham 1 1 lim))

(defn ^longs sol214
  [^long lim ^long nlim]
  (let [refs
        (boolean-array (+ 1 lim) true)

        pfactors
        (fn [^long n]
          (loop [p (int n) i (int 3) r (transient #{})]
            (if (and (not= p 2) (even? p))
              (recur (quot p 2)
                     3
                     (conj! r 2))
              (if (aget refs p)
                (if (== (* p p) n)
                  (persistent! r)
                  (persistent! (conj! r p)))
                (if (== 0 (rem p i))
                  (recur (quot p i)
                         3
                         (conj! r i))
                  (recur p (->> (iterate #(+ 2 %) (+ 2 i))
                                (drop-while #(not (aget refs %)))
                                first) r))))))

        toti (memoize
               (fn toti [^long i]
                 (cond (== i 2) 2
                       (aget refs i) (+ 1 (toti (- i 1)))
                       :else (let [nexi
                                   (let [pts (pfactors i)]
                                     (quot (* i (reduce * (map dec pts)))
                                           (reduce * pts)))]
                               (+ 1 (toti nexi))))))
        llim (int (Math/sqrt lim))]
    (loop [i (int 2) res (long 0)]
      (if (> i lim)
        res
        (if (aget refs i)
          (recur (+ i 1)
                 (do (when (<= i llim)
                       (loop [j (int (* i i))]
                         (if (> j lim)
                           nil
                           (do (aset refs j false)
                               (recur (+ j i))))))
                     (let [itot (toti i)]
                       (if (== itot nlim)
                         (+ res i)
                         res))))
          (recur (+ i 1) res))))))

(defn sol381
  [lim]
  (let [refs (boolean-array (+ 1 lim) true)
        llim (int (Math/sqrt lim))
        modif (fn [n]
                (let [end (- n 5)]
                  (loop [i (int (- n 2))
                         ck (int 1)
                         ts (int 1)
                         res (transient [(- n 1) 1])]
                    (if (== i end)
                      (rem (apply + (persistent! res)) n)
                      (let [tmp
                            (loop [x (if (odd? ts)
                                       (int 2)
                                       (- n 1))]
                              (let [t (long (rem (* x i) n))]
                                (if (== t ck)
                                  x
                                  (recur (if (odd? ts)
                                           (+ x 1)
                                           (- x 1))))))]
                        (recur (- i 1) tmp (+ 1 ts) (conj! res tmp)))))))]
    (loop [i (int 3) res (long 0)]
      (if (> i lim)
        res
        (if (aget refs i)
          (recur (+ i 2)
                 (do (when (<= i llim)
                       (loop [j (int (* i i))]
                         (if (> j lim)
                           nil
                           (do (aset refs j false)
                               (recur (+ j i))))))
                     (if (>= i 5)
                       (+ res (modif i))
                       res)))
          (recur (+ i 2) res))))))

(defn ^long sol
  [^long st ^long lim]
  (let [gcd (fn gcd [a b]
              (if (== 0 (rem a b))
                b
                (loop [i (int a) j (int b)]
                  (cond (== 0 i) j
                        (== 0 j) i
                        (or (== 1 i) (== 1 j)) 1
                        (< i j) (recur j (- j i))
                        :else (recur j (- i j))))))
        modid (fn [m]
                (loop [i (- m 2)]
                  (if (== (rem (* i i) m) 1)
                    i
                    (recur (- i 1)))))]
    (->> (pmap modid (range st (+ 1 lim)))
         (reduce +'))))

(defn sol
  [^long st ^long end]
  (let [modid (fn [^long m]
                (loop [i (- m 1)]
                  (if (== i (rem (* i i) m))
                    i
                    (recur (- i 1)))))]
    (->> (pmap modid (range st (+ 1 end)))
         (reduce +'))))

(defn ^long sol
  [^long tar ^long mul]
  (let [lim (* tar mul)
        primes (transient (sieve lim))]
    (loop [i (int 1) ctr (int 1) res (long 2)]
      (if (>= ctr tar)
        res
        (let [[ctmp rtmp]
              (loop [j (int 0) ctrj (int 0) resj (long 1)]
                (let [pj (primes j)
                      pi (primes i)
                      pjs (* pj pj)]
                  (if (< pjs pi)
                    (recur (+ j 1) (+ ctrj 1) (* resj pjs))
                    [ctrj resj])))]
          (if (== 0 ctmp)
            (recur (+ i 1)
                   (+ ctr 1)
                   (* res (primes i)))
            (recur (+ i 1)
                   (+ ctr ctmp)
                   (* res rtmp))))))))

(defn udiv
  [n]
  (let [num (fact n)
        divs (divisors num)
        unit? (fn [a]
                (== 1 (gcd a (quot num a))))]
    (filter unit? divs)))

(defn tempsol
  [bhn]
  (let [bahan (permute bhn 4)
        fd (fn ([a] (if (== a 0) 1 (/ a)))
             ([a b]
              (if (== 0 b)
                1
                (/ a b)))
             ([a b c]
              (if (== 0 b)
                1
                (if (== 0 c)
                  (/ a b)
                  (/ a b c)))))
        opr [+ - fd *]
        oprs (permutes opr 3)
        res (->> (for [[a b c d] bahan [k l m] oprs]
                   [(k (l a b) (m c d)) (k (l a) (m b c d))
                    (k (l a (m b c)) d) (k (l (m a b) c) d)])
                 (apply concat)
                 (filter #(and (integer? %)
                               (pos? %)))
                 distinct sort)]
    (loop [[x & xs] res prev (int 0) cnt1 (int 0) cnt2 (int 0)]
      (if x
        (if (== x (+ 1 prev))
          (recur xs x (+ 1 cnt1) cnt2)
          (if (> cnt1 cnt2)
            (recur xs x 1 cnt1)
            (recur xs x 1 cnt2)))
        cnt2))))

(defn sol
  []
  (loop [[x & xs] (vec (combine (range 1 10) 4))
         prev (int 0)
         res []]
    (if x
      (let [tmp (tempsol x)]
        (if (> tmp prev)
          (recur xs tmp x)
          (recur xs prev res)))
      [prev res])))

(defn sol
  [lim]
  (let [bhn (range 10)
        start (->> (cons j i)
                   (for [i (permutes bhn 2)
                         j (range 1 10)])
                   (filter #(<= (apply + %) 9)))]
    (loop [i (int 3) res start]
      (if (> i lim)
        (count res)
        (recur (+ i 1)
               (for [r (map #(vec (take 2 %)) res)
                     j bhn
                     :when (<= (+ j (apply + r)) 9)]
                 (conj r j)))))))

(defn sol
  [sqrs]
  (let [bhn (combine (range 10) 6)]
    (->> (hash-set i j)
         (for [i bhn j bhn
               :let [ir (if (some #{9} i)
                          (conj i 6)
                          (if (some #{6} i) (conj i 9) i))
                     jr (if (some #{9} j)
                          (conj j 6)
                          (if (some #{6} j) (conj j 9) j))
                     res (->> (for [xi ir xj jr]
                                [(colnum [xi xj])
                                 (colnum [xj xi])])
                              (apply concat)
                              set)]
               :when (subset? sqrs res)])
         set count)))

(def digs (->> (range 10)
               (mapcat #(list [(str % "a") %]
                              [(str % "b") %]))
               (into {})))

(def sdigs (set (keys digs)))

(def choices (combine (keys digs) 10))

(def selected
  (let [diffs (fn [st]
                (let [anti (difference sdigs st)
                      sumst (int (apply + (map #(get digs %) st)))
                      sumanti (int (apply + (map #(get digs %) anti)))]
                  (- sumst sumanti)))
        div? (fn [st]
               (let [diff (int (diffs st))]
                 (== 0 (rem diff 11))))]
    (sequence
      (comp (filter div?)
            (map #(vector (mapv (fn [x] (get digs x)) %)
                          (mapv (fn [x] (get digs x))
                                (difference sdigs %)))))
      choices)))

(defn sol
  [bahan]
  (let [cprob (fn [[st & stm]]
                (let [czero (count (filter #{0} st))
                      sset (set st)
                      cset (count sset)
                      cstm (count (set stm))]
                  (*' (->> (apply * (repeat (- 10 cset) 2))
                           (quot (* (- 10 czero) (fact 9))))
                      (->> (apply * (repeat (- 10 cstm) 2))
                           (quot (fact 10))))))
        total (->> bahan
                   (group-by #(set (first %)))
                   vals
                   (mapv first))]
    (->> (pmap cprob total)
         (reduce +))))

(defn ^long sol1
  [^long lim]
  (transduce
    (filter #(or (== 0 (rem % 3))
                 (== 0 (rem % 5))))
    + (range lim)))

(defn ^long sol1a
  [^long lim]
  (let [[a b c] (pvalues (apply + (range 3 lim 3))
                         (apply + (range 5 lim 5))
                         (apply + (range 15 lim 15)))]
    (- (+ a b) c)))

















