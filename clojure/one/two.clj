(ns poly-euler.two
  (:require [clojure.set :refer [union difference intersection]]))

(def inc-digits
  (memoize
   (fn [n jumdig]
     (if (= jumdig 1)
       (- 10 n)
       (reduce +' (map #(inc-digits % (dec jumdig)) (range n 10)))))))

(def dec-digits
  (memoize
   (fn [n jumdig]
     (if (= jumdig 1)
       (if (= 0 n)
         1
         0)
       (if (= jumdig 2)
         (if (= 0 n)
           1
           (+ (inc n) (reduce + (range 1 (inc n)))))
         (reduce +' (map #(dec-digits % (dec jumdig))
                         (range n -1 -1))))))))


(defn nth-triangular
  [n r]
  (/ (reduce *' (range n (+ n r)))
     (reduce *' (range 1 (inc r)))))

(defn digits
  [jumdig]
  (+ (->> (nth-triangular 9 j)
          (for [j (range 1 (inc jumdig))])
          (reduce +'))
     (->> (range 1 (inc jumdig))
          (map #(dec-digits 9 %))
          (reduce +)
          (+ (- (* (- jumdig 1) 9)) (- (dec jumdig))))))

(defn psqr?
  [n]
  (let [num (Math/sqrt n)]
    (== num (int num))))

(defn ^longs finda
  [^long lim ^long howmany?]
  (loop [n (int 0) res (int 0)]
    (if (>= n lim)
      res
      (let [resb (loop [k (int (if (even? n) 2 1)) resi (int 0)]
                   (if (<= (* 2 k) n)
                     (let [bsqr (/ (+ (* k k) n) 4)
                           b (int (Math/sqrt bsqr))
                           a1 (+ (* 2 b) k)
                           a2 (- (* 2 b) k)]
                       (if (psqr? bsqr)
                         (recur (+ 2 k)
                                (+ resi (if (<= a2 b) 1 2)))
                         (recur (+ 2 k) resi)))
                     resi))]
        (recur (inc n)
               (if (== resb howmany?)
                 (+ res 1)
                 res))))))


(defn findn
  [lim n nlim]
  (let [tmp (loop [a (int 3) res []]
              (if (> a lim)
                res
                (let [resbi (loop [b (int (Math/ceil (/ a 4))) res []]
                              (let [sel (- a b)
                                    n (- (* 4 a b) (* a a))]
                                (if (and (< sel 0) (> n nlim))
                                  res
                                  (recur (inc b) (conj res n)))))]
                  (recur (inc a) (concat res resbi)))))]
    (->> tmp frequencies (filter #(== (val %) n)))))


(defn findb
  [target lim]
  (loop [n (int 3) res 0]
    (if (> n lim)
      res
      (let [resa (loop [a (int (Math/sqrt (/ n 3))) res 0]
                   (let [b (/ (+ (/ n a) a) 4)]
                     (if (> a n)
                       res
                       (if (and (not= a b) (integer? b))
                         (recur (inc a) (inc res))
                         (recur (inc a) res)))))]
        (if (== resa target)
          (recur (inc n) (inc res))
          (recur (inc n) res))))))

(defn combs
  [^long n ^long k]
  (let [m (int (- n k))]
    (if (> m k)
      (/ (reduce *' (range (inc m) (inc n)))
         (reduce *' (range 1 (inc k))))
      (/ (reduce *' (range (inc k) (inc n)))
         (reduce *' (range 1 (inc m)))))))



(def cases
  (memoize
   (fn [^long i ^long c]
     (cond (== c 1) [{c i}]
           :else
           (loop [x (int 0) res []]
             (if (> (*' x c) i)
               res
               (recur (+ 1 x)
                      (concat res
                              (map #(merge % {c x})
                                   (cases (- i (* x c))
                                          (- c 1)))))))))))

(defn count-one
  [cur]
  (let [tots (reduce + (vals cur))
        pils (filter  #(pos? (val %)) cur)
        cpil (count pils)]
    (loop [[[b cb] & rb] pils counter tots res (int 1)]
      (if (empty? rb)
        (*' res (combs counter cb))
        (recur rb (- counter cb) (*' res (combs counter cb)))))))

(defn count-all
  [lim]
  (loop [[c & cr] (cases lim lim) res 0]
    (if (empty? cr)
      (+ res (count-one c))
      (recur cr (+ res (count-one c))))))

(def cblacks1
  (memoize 
   (fn [^long tots]
     (cond (zero? tots) 1
           (== tots 3) 1
           (<= 1 tots 2) 0
           :else (+ 2 (reduce + (map #(* (cblacks %)
                                         (cblacks (- tots % 1)))
                                     (range 0 (inc (quot tots 2))))))))))

(defn total1
  [^long tots]
  (+ 2 (reduce + (map #(* (cblacks1 %)
                          (cblacks1 (- tots % 1)))
                      (range 0 tots)))))

(defn cblacks
  [^long tots ^long n]
  (combs (rem tots 3) n))




