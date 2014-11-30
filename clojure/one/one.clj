(ns poly-euler.one
  (:require [clojure.set :refer [union difference intersection]]))

(set! *unchecked-math* true)

(defn expn
  [a m]
  (if (= 1 m)
    a
    (if (= 0 (rem m 2))
      (expn (*' a a) (quot m 2))
      (*' a (expn (* a a) (quot (- m 1) 2))))))

(defn ^long find-cycle
  [^long n]
  (let [refs (boolean-array (+ 1 n))
        refs2 (boolean-array (+ 1 n))]
    (loop [i (int 1) res (int 0) res2 (int 0)]
      (if (aget refs2 i)
        res2
        (let [rems (int (rem (* 10 i) n))]
          (if (== 0 rems)
            0
            (if (aget refs i)
              (do (aset refs2 i true)
                  (recur rems res (+ 1 res2)))
              (do (aset refs i true)
                  (recur rems (+ 1 res) res2)))))))))

(defn ^long max-cycle
  [^long n]
  (loop [i (int n) p (int n) res (int 0)]
    (if (> res i)
      [p res]
      (let [tmp (int (find-cycle i))]
        (if (> tmp res)
          (recur (- i 1) i tmp)
          (recur (- i 1) p res))))))

(defn third [ls] (nth ls 2))

(defn ^longs euler28
  [^long lim]
  (->> (iterate #(let [[a b c] %
                       one (+ a (+ 8 (- a b)))
                       res (+ (* 4 one) c)]
                   [one a res])
                [6 1 25])
       (drop (dec (quot lim 2)))
       first third))

(defn ^long euler28a
  [^long lim]
  (->> (range (int 3) (inc (int lim)) (int 2))
       (pmap #(* 2 (+ (* % %) (- (* % %) (* 3 (- % 1))))))
       (reduce +) (+ 1)))

(defn ^long euler28b
  [^long lim]
  (loop [a (int 6) b (int 1) res (int 25) i (int 3)]
    (if (== i lim)
      res
      (let [tempa (+ a (+ 8 (- a b)))]
        (recur tempa a (+ res (* 4 tempa)) (+ 2 i))))))

(defn ^boolean prime?
  [^long p]
  (cond (< p 2) false
        (== 2 p) true
        (== 0 (rem p 2)) false
        :else (let [lim (+ 1 (int (Math/sqrt p)))]
                (loop [i (long 3)]
                  (if (> i lim)
                    true
                    (if (== 0 (rem p i))
                      false
                      (recur (+ i 2))))))))

(defn ^long next-prime
  [^long p]
  (cond (== p 2) 3
        (prime? (+ p 2)) (+ p 2)
        :else (next-prime (+ p 2))))

(defn ^long prev-prime
  [^long p]
  (cond (<= p 2) nil
        (== p 3) 2
        (prime? (- p 2)) (- p 2)
        :else (prev-prime (- p 2))))

(defn ^long sieves
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        refs (boolean-array (inc lim))]
    (loop [i (int 3) res (transient [2])]
      (if (>= i lim)
        (persistent! res)
        (recur (if (and (<= i llim) (not (aget refs i)))
                 (loop [p (int (* i i))]
                   (if (<= p (- lim 1))
                     (recur (do (aset refs p true)
                                (+ p (* 2 i))))
                     (+ 2 i)))
                 (+ 2 i))
               (if (aget refs i) res (conj! res i)))))))

(defn ^long euler27a
  [^long lim]
  (->> (for [b (sieves lim)]
         (loop [a (filter #(> (+ 1 % b) 0) (range (- lim) (inc lim)))
                cura (int (- lim)) res (int 1)]
           (if (empty? a)
             [res cura b]
             (let [resn (int (loop [n (int 1) res (int 1)]
                               (if (prime? (+ (* n n)
                                              (* (first a) n)
                                              b))
                                 (recur (+ 1 n) (+ 1 res))
                                 res)))]
               (if (> resn res)
                 (recur (rest a) (first a) resn)
                 (recur (rest a) cura res))))))
       (sort-by first) last rest (apply *)))

(defn ^long euler27
  [^long lim]
  (loop [b 2 [rr ar br & _] [0,0,0]]
    (if (> b lim)
      (* ar br)
      (let [[rar aar bar & _]
            (loop [a (int (- lim)) cura (int (- lim)) resa (int 1)]
              (cond
               (> a lim) [resa cura b]
               (<= (+ a b 1) 0) (recur (+ 1 a) cura resa)
               :else (let [resn (int (loop [n (int 1) res (int 1)]
                                       (if (prime? (+ (* n n)
                                                      (* a n)
                                                      b))
                                         (recur (+ 1 n) (+ 1 res))
                                         res)))]
                       (if (> resn resa)
                         (recur (+ 1 a) a resn)
                         (recur (+ 1 a) cura resa)))))]
        (if (> rar rr)
          (recur (next-prime b) [rar aar bar])
          (recur (next-prime b) [rr ar br]))))))

(defn ^long euler27b
  [^long lim]
  (loop [b 997 [rr ar br & _] [0,0,0]]
    (if (< b rr)
      (* ar br)
      (let [[rar aar bar & _]
            (loop [a (int (- lim 1)) cura (int (- lim)) resa (int 1)]
              (cond
               (< a (- lim)) [resa cura b]
               (<= (+ a b 1) 0) (recur (- a 2) cura resa)
               :else (let [resn (int (loop [n (int 1) res (int 1)]
                                       (if (prime? (+ (* n n)
                                                      (* a n)
                                                      b))
                                         (recur (+ 1 n) (+ 1 res))
                                         res)))]
                       (if (> resn resa)
                         (recur (- a 2) a resn)
                         (recur (- a 2) cura resa)))))]
        (if (> rar rr)
          (recur (prev-prime b) [rar aar bar])
          (recur (prev-prime b) [rr ar br]))))))

(defn ^long pow
  [^long a ^long m]
  (loop [a (int a) m (int m) res (int 1)]
    (if (== m 0) res (recur a (- m 1) (* a res)))))

(defn ^long sumfif
  [^long n]
  (loop [i (int n) res (int 0)]
    (if (< i 10)
      (+ res (pow i 5))
      (recur (quot i 10) (+ res (pow (rem i 10) 5))))))

(defn ^long euler30
  []
  (loop [i (int (* 6 (pow 9 5))) res (int 0)]
    (if (== i 10)
      res
      (let [sum5 (int (sumfif i))]
        (if (== sum5 i)
          (recur (- i 1) (+ res i))
          (recur (- i 1) res))))))

(defn ^long euler30p
  [^long lim]
  (letfn [(sum-fifth?
            [^long n]
            (loop [i (int n) res (int 0)]
              (if (> res n)
                false
                (if (< i 10)
                  (== n (+ res (pow i 5)))
                  (recur (quot i 10) (+ res (pow (rem i 10) 5)))))))
          (sumup [^long n]
            (let [lim (int (* 1000 (+ n 1)))]
              (loop [i (int (* 1000 n)) res (int 0)]
                (if (== i lim)
                  res
                  (if (sum-fifth? i)
                    (recur (+ 1 i) (+ res i))
                    (recur (+ 1 i) res))))))]
    (reduce + (pmap sumup (range lim)))))

(defn ^long euler30r
  [^long lim]
  (letfn [(sum-fifth?
            [^long n]
            (loop [j (int n) res (int 0)]
              (if (> res n)
                false
                (if (< j 10)
                  (== n (+ res (pow j 5)))
                  (recur (quot j 10) (+ res (pow (rem j 10) 5)))))))]
    (loop [i (int lim) res (int 0)]
      (if (== i 10)
        res
        (if (sum-fifth? i)
          (recur (- i 1) (+ res i))
          (recur (- i 1) res))))))

(def coins [1,2,5,10,20,50,100,200])

(def sumas
  (memoize
   (fn [^long i ^long c]
     (cond (== i 0) 1 
           (== c 0) 1
           :else (->> (iterate inc 0)
                      (take-while #(<= (* % (nth coins c)) i))
                      (map #(sumas (- i (* % (nth coins c)))
                                   (- c 1)))
                      (reduce +))))))

(defn ^long suma-coins
  [^long n]
  (sumas n 7))

(def isuma
  (memoize
   (fn [^long i ^long c]
     (cond (== i 0) 1 
           (== c 1) 1
           :else (->> (iterate inc 0)
                      (take-while #(<= (* % c) i))
                      (map #(isuma (- i (* % c))
                                   (- c 1)))
                      (reduce +))))))

(defn ^long suma-ints
  [^long n]
  (dec (isuma n 100)))

(defn ^long sum-changes
  [^long n]
  (let [sumal (memoize
               (fn sumal [^long i ^long c]
                 (cond (== i 0) 1 
                       (== c 0) 1
                       :else (->> (iterate inc 0)
                                  (take-while #(<= (* % (nth coins c)) i))
                                  (map #(sumal (- i (* % (nth coins c)))
                                               (- c 1)))
                                  (reduce +))))) ]
    (sumal n 7)))

(defn sumap
  [^long i ^long c]
  (cond (== i 0) 1 
        (== c 0) 1
        :else (->> (iterate inc 0)
                   (take-while #(<= (* % (nth coins c)) i))
                   (map #(sumap (- i (* % (nth coins c)))
                                (- c 1)))
                   (reduce +))))

(defn ^long suma-coins2
  [^long n]
  (sumap n 7))

(def cs [1,2,5,10,20,50,100,200])

(def sumas3
  (fn [^long i ^long c]
    (cond (== i 0) 1 
          (== c 0) 1
          :else
          (loop [x (int 0) res (int 0)]
            (if (> (* x (nth cs c)) i)
              res
              (recur (+ 1 x)
                     (+ res (sumas3 (- i (* x (nth cs c)))
                                    (- c 1)))))))))

(defn ^long suma-coins3
  [^long n]
  (sumas n 7))
(def prime'
  (memoize
   (fn prime' [^long p]
     (cond (< p 2) false
           (== 2 p) true
           (== 0 (rem p 2)) false
           :else (let [lim (+ 1 (int (Math/sqrt p)))]
                   (loop [i (int 3)]
                     (if (> i lim)
                       true
                       (if (== 0 (rem p i))
                         false
                         (recur (+ i 2))))))))))



(def prev-prime
  (memoize
   (fn prev-prime [^long p]
     (cond (<= p 2) 0
           (== p 3) 2
           (== 0 (rem p 2)) (prev-prime (+ p 1))
           (prime' (- p 2)) (- p 2)
           :else (prev-prime (- p 2))))))



(def isuma2
  (memoize
   (fn [^long i ^long c]
     (cond (== i 0) 1 
           (== c 1) 1
           :else
           (loop [x (int 0) res (int 0)]
             (if (> (* x c) i)
               res
               (recur (+ 1 x)
                      (+ res (isuma2 (- i (* x c))
                                     (- c 1))))))))))

(defn ^long suma-ints2
  [^long n]
  (dec (isuma2 n 100)))

(def psuma
  (memoize
   (fn [^long i ^long c]
     (cond (== 1 i) 0
           (== c 2) (if (== 0 (rem i 2)) 1 0)
           :else
           (loop [x (int 0) res (int 0)]
             (if (> (* x c) i)
               res
               (recur (+ 1 x)
                      (+ res (psuma (- i (* x c))
                                     (prev-prime c))))))))))

(defn ^long prime-sum
  [^long n ^long target]
  (loop [i (int n)]
    (let [psum  (psuma i (prev-prime i))]
      (if (> psum target)
        [i psum]
        (recur (inc i))))))

(defn strange-loop
  [n]
  (loop [i 1 res (map vector (range 10))]
    (if (== i n)
      res
      (recur (inc i)
             (concat res
                     (for [m res
                           j (range 10)]
                       (conj m j)))))))

(defn removes
  [e ls]
  (remove #(= e %) ls))

(defn permutations
  [ls]
  (if (== 1 (count ls))
    (map vector ls)
    (for [mat ls
          pres (permutations (removes mat ls))]
      (cons mat pres))))

(defn colnum
  [ls]
  (loop [[x & xs] ls res 0]
    (if (empty? xs)
      (+ (* 10 res) x)
      (recur xs (+ (* 10 res) x)))))

(defn ^long pandig-prime
  [^long n]
  (let [raw (range n 0 -1)]
    (loop [i (int (- n 1))]
      (if (== 1 i)
        nil
        (let [tmp (take i raw)
              res (->> (drop i raw)
                       (permutations)
                       (map #(colnum (concat tmp %)))
                       (filter prime?))]
          (if (empty? res)
            (recur (- i 1))
            (apply max res)))))))

(defn pandig?
  "Simple pandigital checking"
  [^longs ls]
  (= (sort ls) (range 1 10)))

(defn numcol
  "Converting number into list of digits"
  [n]
  (loop [i n res '()]
    (if (< i 10)
      (cons i res)
      (recur (quot i 10)
             (cons (rem i 10) res)))))

(defn ^longs pandig-products
  "Sum all pandigital products"
  [lim]
  (->> (range 2 (* 3 lim))
       (pmap #(loop [j (int (+ 1 %)) resj []]
                (if (> (* % j) (* 3 lim))
                  resj
                  (recur (+ 1 j)
                         (let [pandig (concat (numcol %)
                                              (numcol j)
                                              (numcol (* % j)))]
                           (if (pandig? pandig)
                             (conj resj (* % j))
                             resj))))))
       (apply concat) distinct (reduce +)))

(defn ^longs pandig-products2
  [lim]
  (loop [i (int 2) res []]
    (if (> (* i i) lim)
      (reduce + (distinct res))
      (recur (+ 1 i)
             (concat res
                     (loop [j (int (+ 1 i)) resj []]
                       (if (> (* i j) (* 3 lim))
                         resj
                         (recur (+ 1 j)
                                (let [pandig (concat (numcol i)
                                                     (numcol j)
                                                     (numcol (* i j)))]
                                  (if (pandig? pandig)
                                    (conj resj (* i j))
                                    resj))))))))))

(defn circular-prime?
  [^long n]
  (let [res (int (dec (count (numcol n))))]
    (if (== res 0)
      (some #(== % n) [3 7])
      (loop [i res m n]
        (if (== i -1)
          true
          (if (prime' m)
            (recur (- i 1)
                   (+ (quot m 10)
                      (* (rem m 10) (pow 10 res))))
            false))))))

(defn ^long all-cprimes2
  [^long lim]
  (let [bahan [1 3 7 9]
        looper (fn looper [^long i]
                 (if (> i lim)
                   0
                   (if (circular-prime? i)
                     (->> bahan
                          (map #(looper (colnum (cons % (numcol i)))))
                          (reduce +)
                          (+ 1))
                     (->> bahan
                          (map #(looper (colnum (cons % (numcol i)))))
                          (reduce +)))))]
    (+ 2 (reduce + (map looper bahan)))))

(defn ^long all-cprimes
  [^long lim]
  (let [bahan [1 3 7 9]
        looper (fn looper [^long i]
                 (if (> i lim)
                   0
                   (if (circular-prime? i)
                     (->> bahan
                          (map #(looper (+ % (* 10 i)) ))
                          (reduce +)
                          (+ 1))
                     (->> bahan
                          (map #(looper (+ % (* 10 i))))
                          (reduce +)))))]
    (+ 2 (reduce + (pmap looper bahan)))))

(defn bin-palin?
  "Check whether a number is palindrome in binary base"
  [^long n]
  (letfn [(bincol [^long n]
            (loop [i (int n) res []]
              (if (< i 2)
                (conj res i)
                (recur (quot i 2)
                       (conj res (rem i 2))))))]
    (let [res (bincol n)]
      (= res (reverse res)))))

(defn ^long bi-palins
  "Generate all n-digit palindromes, filter whether they are also
  palindrome in base 2, and sum the results"
  [^long n]
  (if (== n 1)
    (->> (range 1 10 2)
         (filter bin-palin?)
         (reduce +))
    (let [expn (int (- (quot n 2) 1))
          start (int (pow 10 expn))
          end (int (pow 10 (inc expn)))]
      (if (even? n)
        (loop [i start res 0]
          (if (>= i end)
            res
            (recur (+ i 1)
                   (let [tmp (numcol i)
                         num (colnum (concat tmp (reverse tmp)))]
                     (if (== 0 (rem num 2))
                       res
                       (if (bin-palin? num)
                         (+ res num)
                         res))))))
        (loop [i start res 0]
          (if (>= i end)
            res
            (recur (+ i 1)
                   (let [tmp (numcol i)
                         rtmp (reverse tmp)
                         nums (->> (range 10)
                                   (pmap #(colnum (concat tmp [%] rtmp))))]
                     (->> (filter odd? nums)
                          (filter bin-palin?)
                          (reduce +)
                          (+ res))))))))))

(defn ^long all-bipalins
  "Generate all n-digit palindromes, filter whether they are also
  palindrome in base 2, and sum the results"
  [^long n]
  (if (== n 1)
    (->> (range 1 10 2)
         (reduce +)) 
    (let [expn (int (- (quot n 2) 1))
          start (int (pow 10 expn))
          end (int (pow 10 (+ 1 expn)))]
      (if (== 0 (rem n 2))
        (loop [i (int start) res (int 0)]
          (if (>= i end)
            res
            (let [tmp (numcol i)]
              (if (== 0 (rem (first tmp) 2))
                (recur (colnum (cons (+ 1 (first tmp)) (rest tmp))) res)
                (let [num (int (colnum (concat tmp (reverse tmp))))]
                  (if (bin-palin? num)
                    (recur (+ 1 i) (+ res num))
                    (recur (+ 1 i) res)))))))
        (loop [i (int start) res (int 0)]
          (if (>= i end)
            res
            (let [tmp (numcol i)]
              (if (== 0 (rem (first tmp) 2))
                (recur (colnum (cons (+ 1 (first tmp)) (rest tmp))) res)
                (let [nums (map #(colnum (concat tmp [%] (reverse tmp)))
                                (range 10))
                      tnums (reduce + (filter bin-palin? nums))]
                  (recur (+ 1 i) (+ res tnums)))))))))))

(defn ^long sum-bipalins
  "Returns all double bases palindromes up-to 10^n"
  [^long n]
  (reduce + (pmap #(all-bipalins %) (range 1 (inc n)))))

(defn rt-prime?
  [^long n]
  (if (< n 10)
    (some  #(== % n) (list 2 3 5 7))
    (if (prime' n)
      (rt-prime? (quot n 10))
      false)))

(defn lt-prime?
  [^long n]
  (if (< n 10)
    (some #(== % n) (list 2 3 5 7))
    (if (prime? n)
      (lt-prime? (rem n (pow 10 (- (count (numcol n)) 1))))
      false)))

(def lprime?
  (memoize
   (fn [^long n]
     (and (lt-prime? n) (rt-prime? n)))))

(defn ^long lsieves
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        refs (boolean-array (inc lim))]
    (loop [i (int 3) res (transient [2]) resta (transient [])]
      (if (>= i lim)
        (reduce + (drop 3 (persistent! resta)))
        (recur (if (and (<= i llim) (not (aget refs i)))
                 (loop [p (int (* i i))]
                   (if (<= p (- lim 1))
                     (recur (do (aset refs p true)
                                (+ p (* 2 i))))
                     (+ 2 i)))
                 (+ 2 i))
               (if (aget refs i) res (conj! res i))
               (if (aget refs i)
                 resta
                 (if (lprime? i) (conj! resta i) resta)))))))

(defn ^longs iter-tls
  [^long lim]
  (let [lefts [2 3 5 7]
        rights [3 7]
        mids [1 3 7 9]
        looper (fn [^longs ms]
                 (->> [(colnum (concat [i] (conj ms mms) [j]))
                       (colnum (concat [i] (cons mms ms) [j]))]
                      (for [i lefts j rights mms mids])
                      (apply concat)
                      (filter lprime?)
                      (into #{})))]
    (reduce +
            (loop [res #{23 37 53 73} lms #{[]}]
              (if (>= (count res) lim)
                res
                (recur (union res (apply union (map looper lms)))
                       (->> [(cons mset iset) (conj iset mset)]
                            (for [iset lms mset mids])
                            (apply concat)
                            (into #{}))))))))

(defn ^long iter-tls2
  [^long lim]
  (let [digs [1 3 7 9]]
    (loop [res #{} refs [2 3 5 7]]
      (if (>= (count res) lim)
        (reduce + res)
        (let [resta (loop [i (int 0) resa1 [] resa #{}]
                      (if (== i 4)
                        [resa1 resa]
                        (let [refta (loop [refsta refs resb [] resb1 #{}]
                                      (if (empty? refsta)
                                        [resb resb1]
                                        (let [tmp (int (+ (digs i)
                                                          (* 10 (first refsta))))]
                                          (if (rt-prime? tmp)
                                            (if (lt-prime? tmp)
                                              (recur (rest refsta)
                                                     (conj resb tmp)
                                                     (conj resb1 tmp))
                                              (recur (rest refsta)
                                                     (conj resb tmp)
                                                     resb1))
                                            (recur (rest refsta)
                                                   resb
                                                   resb1)))))]
                          (recur (+ i 1)
                                 (concat resa1 (first refta))
                                 (union resa (second refta))))))]
          (recur (union res (second resta))
                 (first resta)))))))

(defn ^long truncatable-primes
  [^long lim]
  (let [digs [1 3 7 9]
        looper (fn looper [[a b]]
                 (let [tmp (for [idig digs iadd a]
                             (int (+ (* 10 iadd) idig)))
                       tmp1 (filter #(rt-prime? %) tmp)]
                   [tmp1 (union b (into #{} (filter lt-prime? tmp1)))]))]
    (->> [[2 3 5 7] #{}]
         (iterate looper)
         (drop-while #(< (count (second %)) lim))
         first second (reduce +))))

(defn ^long tprimes
  "Returns the sum of all truncatable primes, all 11 of them :)"
  [^long lim]
  (let [digs [1 3 7 9]
        looper (fn looper [ms]
                 (loop [j (int 0) res (transient [])]
                   (if (== 4 j)
                     (persistent! res)
                     (let [tmp (+ (nth digs j) (* 10 ms))]
                       (if (rt-prime? tmp)
                         (recur (+ 1 j) (conj! res tmp))
                         (recur (+ 1 j) res))))))]
    (loop [refs [2 3 5 7] res []]
      (if (>= (count res) lim)
        (reduce + res)
        (let [tmp (mapcat looper refs)]
          (recur tmp (concat res (filter lt-prime? tmp))))))))

(def  count-factors
  (memoize
   (fn [^long n]
     (let [lim (int (inc (Math/sqrt n)))]
       (if (even? n)
         (loop [i (int 2) res (int 2)]
           (if (> i lim)
             res
             (let [divs (quot n i)]
               (if (== 0 (rem n i))
                 (if (== i divs)
                   (inc res)
                   (recur (inc i) (+ 2 res)))
                 (recur (inc i) res)))))
         (loop [i (int 3) res (int 2)]
           (if (> i lim)
             res
             (let [divs (quot n i)]
               (if (== 0 (rem n i))
                 (if (== i divs)
                   (inc res)
                   (recur (+ 2 i) (+ 2 res)))
                 (recur (+ 2 i) res))))))))))

(defn ^long triangle500
  [^long target]
  (time
   (loop [i (int 3)]
     (let [vals (if (== 0 (rem i 2))
                  (* (count-factors (quot i 2))
                     (count-factors (+ i 1)))
                  (* (count-factors (quot (+ i 1) 2))
                     (count-factors i)))]
       (if (> vals target)
         (/ (* (+ i 1) i) 2)
         (recur (+ 1 i)))))))

(defn ^long champers
  [^long n]
  (time (reduce * (pmap #(nth (mapcat numcol (iterate inc 1)) (dec (pow 10 %))) (range n)))))

(defn ^longs permute
  "Permutations of n element from a list of elements in ls"
  [^long n ^longs ls]
  (if (== n 1)
    (map vector ls)
    (mapcat (fn [s] (map #(cons s %) (permute (- n 1) (removes s ls)))) ls)))

(defn ^longs pandig-prod
  "Returns the list of pandigital products produced by concatenating a number with [1,2..]"
  [^long n]
  (time
   (let [mat [2 3 6 7]
         create (fn [ls]
                  (let [num (cons 9 ls)]
                    (concat num (numcol (* 2 (colnum num))))))]
     (loop [xs (permute n mat) res []]
       (if (empty? xs)
         res
         (recur (rest xs)
                (let [numbro (create (first xs))]
                  (if-not (pandig? numbro)
                    res
                    (conj res (colnum numbro))))))))))

(defn pita1000a
  [n]
  (->> (for [a (range 3 (inc (quot n 4)))
             b (range a (inc (quot n 2)))
             :let [c (+ (* a a) (* b b))
                   csqrt (Math/sqrt c)
                   side (+ a b (int csqrt))]
             :when (and (== csqrt (int csqrt))
                        (<= side n))]
         side)
       frequencies (sort-by val) last time))

(defn ^longs pita1000
  [^long n]
  (->> (for [b (range a (inc (quot n 2)))
             :let [c (int (+ (* a a) (* b b)))
                   csqrt (Math/sqrt c)
                   side (+ a b (int csqrt))]
             :while (<= side n)
             :when (== csqrt (int csqrt))]
         side)
       (for [a (range 3 (inc (quot n 4)))])
       (apply concat)
       frequencies (sort-by val) last time))

(defn ^longs factors
  [^long n]
  (let [start (if (even? n) 2 3)
        step (if (even? n) 1 2)]
    (loop [i (int start) counter (int 2) res [1 n]]
      (if (>= (* i i) n)
        (if (== (* i i) n)
          (let [[a b] (split-at (quot counter 2) res)]
            (concat a (list i) b))
          res)
        (if (== 0 (rem n i))
          (recur (+ i step)
                 (+ 2 counter)
                 (let [[a b] (split-at (quot counter 2) res)]
                   (concat a (list i (quot n i)) b)))
          (recur (+ i step) counter res))))))

(defn ^longs factors1
  [^long n]
  (let [start (if (even? n) 2 3)
        step (if (even? n) 1 2)]
    (loop [i (int start) res [1 n]]
      (if (>= (* i i) n)
        (if (== (* i i) n)
          (sort (conj res i))
          (sort res))
        (if (== 0 (rem n i))
          (recur (+ i step)
                 (conj res i (quot n i)))
          (recur (+ i step) res))))))

(defn pita1000b
  [^long lim]
  (->> (loop [a (int 3) res {}]
         (if (> a (inc (quot lim 4)))
           res
           (let [tres1 (loop [[d & dres] (factors a) res {}]
                         (let [asqr (int (* a a))
                               b (/ (- (/ asqr d) d) 2)]
                           (if (> a b)
                             res
                             (let [c (+ b d)
                                   peri (+ a b c)]
                               (if (> peri lim)
                                 (recur dres res)
                                 (recur dres
                                        (if (integer? b)
                                          (merge-with + res {peri 1})
                                          res)))))))]
             (recur (+ 1 a) (merge-with + res tres1)))))
       (sort-by val) time))

(defn single-pitas
  [^long lim]
  (->> (loop [a (int 3) res []]
         (if (> a (inc (quot lim 4)))
           res
           (let [faca (factors1 a)
                 bahan (->> (for [m faca]
                              (for [k faca
                                    :let [amk (* m k)]
                                    :while (<= amk a)]
                                amk))
                            (apply concat)
                            distinct
                            sort)
                 tres (loop [[d & dres] bahan res []]
                        (let [asqr (int (* a a))
                              b (/ (- (/ asqr d) d) 2)]
                          (if (> a b)
                            res
                            (let [c (+ b d)
                                  peri (+ a b c)]
                              (if (> peri lim)
                                (recur dres res)
                                (recur dres
                                       (if (integer? b)
                                         (conj res peri)
                                         res)))))))]
             (recur (+ 1 a) (concat res tres)))))
       frequencies (sort-by val) last time))

(defn gcd
  "Accepts two numbers and returns the greatest common divisors of
  those numbers"
  [a b]
  (loop [i (int a) j (int b)]
    (if (= i j)
      i
      (if (> i j)
        (recur j (- i j))
        (recur i (- j i))))))

(defn ^longs pitas
  [^long lim]
  (->> (let [refs (int-array (inc lim) 0)]
         (do (loop [m (int 2)]
               (if (> m (/ lim 2))
                 nil
                 (recur (do (loop [n (int 1)]
                              (if (== n m)
                                nil
                                (if (and (or (even? m) (even? n))
                                         (== 1 (gcd m n)))
                                  (let [a (- (* m m) (* n n))
                                        b (* 2 m n)
                                        c (+ (* m m) (* n n))
                                        peri (+ a b c)]
                                    (if (> peri lim)
                                      nil
                                      (recur (do (loop [idx (int peri)]
                                                   (if (> idx lim)
                                                     nil
                                                     (let [tmp (aget refs idx)]
                                                       (do (aset refs idx (+ tmp 1))
                                                           (recur (+ idx peri))))))
                                                 (+ n 1)))))
                                  (recur (+ n 1)))))
                            (+ m 1)))))
             (loop [idx (int 12) counter 0 res [0 0]]
               (if (> idx lim)
                 [counter res]
                 (let [tmp (aget refs idx)]
                   (recur (+ idx 2)
                          (if (== tmp 1)
                            (+ counter 1)
                            counter)
                          (if (> tmp (second res))
                            [idx tmp]
                            res)))))))
       time))

(defn tripen
  [lim]
  (for [i (range 1 lim)
        j (range 1 lim)
        :let [tri (/ (* i (+ i 1)) 2)
             pen (/ (* j (- (* 3 j) 1)) 2)]
        :when (== tri pen)]
    [i j tri]))

(defn hexagonals
  [lim]
  (map #(* % (dec (* 2 %))) (range 1 lim)))

(defn triangle?
  [n]
  (let [isqr (int (Math/floor (Math/sqrt (* 2 n))))]
    (== (* 2 n) (* isqr (+ 1 isqr)))))

(defn hexal?1
  [n]
  (let [isqr (int (Math/floor (Math/sqrt (/ n 2))))]
    (or (== n (* isqr (- (* 2 isqr) 1)))
        (== n (* (inc isqr) (- (* 2 (inc isqr)) 1))))))

(def hexal?
  (memoize
   (fn [n]
     (let [res (/ (+ 1 (Math/sqrt (+ 1 (* 8 n)))) 4)]
       (== (int res) res)))))

(defn ^longs pentagonals []
  (pmap #(/ (* % (- (* 3 %) 1)) 2) (iterate inc 1)))
(defn euler45
  [n]
  (->> (pentagonals)
       (filter hexal?)
       (take n)
       time))


(def pental?
  (memoize
   (fn [n]
     (let [num (/ (inc (Math/sqrt (inc (* 24 n)))) 6)] (== (int num) num)))))

(defn sumsub-pental? [[x & res]]
  (let [num (filter #(and (pental? (+ x %)) (pental? (- x %))) res)]
    (if (empty? num) nil [x (first num)])))

(defn find-pental []
  (->> (pentagonals) (reductions #(cons %2 %1) '())
       (drop 2) (pmap #(sumsub-pental? %)) (keep identity) first (reduce -)))

(defn divpandig
  []
  (let [digs (into #{} (range 10))
        raw [13 11 7 5 3 2]
        looper (fn looper [^long i cls rdigs]
                 (if (> i 5)
                   (list (concat (into '() (difference digs cls)) cls))
                   (let [p (int (nth raw i))]
                     (->> (map #(cons % (take 2 cls)) (into [] rdigs))
                          (filter #(== 0 (rem (colnum %) p)))
                          (filter #(= (rest %) (take 2 cls)))
                          (mapcat #(looper (inc i)
                                           (cons (first %) cls)
                                           (difference
                                            rdigs
                                            (into #{} (cons (first %) cls)))))))))]
    (->> (permute 3 digs)
         (filter #(== 0 (rem (colnum %) 17)))
         (pmap #(looper 0 % (difference digs (into #{} %))))
         (apply concat)
         (pmap colnum)
         (reduce +)
         time)))

(defn self-power
  [^long lim]
  (rem (reduce +' (pmap #(expn % %) (range 1N (inc lim)))) 10000000000N))

(defn conseq-primes
  [lim]
  (->> (sieves lim)
       (iterate rest)
       (take-while not-empty)
       (pmap #(let [num (take-while not-empty (iterate butlast %))]
                (pmap (fn [nnum] (vector (apply + nnum) (count nnum))) num)))
       (apply concat)
       (sort-by second >)
       (map first)
       (filter prime?)
       first time))

(def square?
  (memoize
   (fn [n]
     (let [nsqr (Math/sqrt n)]
       (== nsqr (int nsqr))))))

(def octa?
  (memoize
   (fn [n]
     (let [num (/ (+ 2 (Math/sqrt (+ 4 (* 24 n)))) 6)]
       (== num (int num))))))

(def hepta?
  (memoize
   (fn [n]
     (let [num (/ (+ 3 (Math/sqrt (+ 9 (* 40 n)))) 10)]
       (== num (int num))))))

(defn which-one?
  [n]
  (loop [i ['triangle? 'square? 'pental? ]]))

(defn pentals
  [n]
  (->> (iterate inc 1)
       (pmap #(/ (* % (- (* 3 %) 1)) 2))
       (drop-while #(< % (expn 10 (dec n))))
       (take-while #(< % (expn 10 n)))))

(defn octals
  [n]
  (->> (iterate inc 1)
       (pmap #(* % (- (* 3 %) 2)))
       (drop-while #(< % (expn 10 (dec n))))
       (take-while #(< % (expn 10 n)))))

(defn squares
  [n]
  (->> (iterate inc 1)
       (pmap #(* % %))
       (drop-while #(< % (expn 10 (dec n))))
       (take-while #(< % (expn 10 n)))))

(defn jait
  [n m]
  (+' (*' 100 n) m))

(defn depan
  [n]
  (quot n 100))

(defn bel
  [n]
  (rem n 100))

(defn find-figures
  [ms ls]
  (let [bahan (range 10 100)]
    (loop [res ms fs ls]
      (if (empty? res)
        nil
        (if (empty? fs)
          res
          (let [tmp (for [m res
                          b bahan
                          :let [bela (bel m)]
                          :when (and (>= bela 10)
                                     ((first fs) (jait bela b)))]
                      (jait m b))]
            (recur tmp (rest fs))))))))

(defn mreduce
  [n]
  (if (< n 10000)
    n
    (+ (rem n 10000) (mreduce (quot n 100)))))

(defn euler61
  [n]
  (time (let [baha [hepta? hexal? pental? square? triangle?]
              bahan [hepta? hexal? pental? square? triangle?]]
          (->> (permutations bahan)
               (pmap #(find-figures (octals n) %))
               (apply concat)
               (filter #(let [num (numcol %)]
                          (= (take 2 num) (take-last 2 num))))
               first mreduce))))

(defn ndigit
  [n]
  (if (or (== 1 n) (== 0 (rem n 10)))
    (int (Math/floor (Math/log10 n)))
    (int (Math/ceil (Math/log10 n)))))

(defn euler63
  []
  (time
   (loop [i (int 4) res (int 3)]
     (if (> i 9)
       res
       (recur (inc i)
              (+ res
                 (loop [j (int 2) resj 1]
                   (let [rslt (int (Math/ceil (* j (Math/log10 i))))]
                     (if (< rslt j)
                       resj
                       (recur (inc j)
                              (if (== rslt j)
                                (+ 1 resj)
                                resj)))))))))))

(defn ndig-cubes2
  [n]
  (if (== n 1)
    [[1 [1]] [8 [8]]]
    (let [end (long (expn 10 n))
          start (long (Math/ceil (Math/pow (expn 10 (dec n)) 1/3)))]
      (->> (iterate inc start)
           (pmap #(let [num (* % % %)]
                    [num (sort (numcol num))]))
           (take-while #(< (first %) end))))))

(defn ndig-cubes
  [^long n]
  (if (== n 1)
    [[1 [1]] [8 [8]]]
    (let [end (long (expn 10 n))
          start (long (Math/ceil (Math/pow (expn 10 (dec n)) 1/3)))]
      (loop [i start res []]
        (let [num (* i i i)]
          (if (> num end)
            res
            (recur (inc i) (conj res [num (sort (numcol num))]))))))))

(defn ^long cube-permutations
  [target]
  (loop [i (int 1)]
    (let [tmp (->> (ndig-cubes i)
                   (group-by second)
                   (filter #(== target (count (val %)))))]
      (if (empty? tmp)
        (recur (inc i))
        (->> (map #(ffirst (second %)) tmp)
             (sort) (first))))))




