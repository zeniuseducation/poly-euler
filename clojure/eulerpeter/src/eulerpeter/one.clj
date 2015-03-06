(ns eulerpeter.one
  (require [eulerpeter.core :refer :all]))


;;Problem 50

(defn next-sum-prime [n]
  (let [[sum i] (sum-prime-until n) j (next-prime i)] (+ sum j)))

(defn eul50 [lim]
  (loop [i 2 res 0]
    (if (< lim res)
      res
      (recur (next-sum-prime i) (if (and (prime? i) (cons-prime-sum? i))
                                  i
                                  res)))))
;;Problem 21
;;Amicable numbers

(defn sum-fak
  ;; sum all the faktor of x without x
  [x]
  (- (sum (faktor x)) x))

(defn ami?
  [x]
  (= x (sum-fak (sum-fak x))))


(defn eul21
  [lim]
  (loop [x 1 res []]
    (if (< lim x)
      res
      (if (some #(= x %) res)
        (recur (inc x) res)
        (recur (inc x) (if (ami? x)
                         (if (= x (sum-fak x))
                           res
                           (conj res x (sum-fak x)))
                         res))))))

;;"Elapsed time: 244.031441 msecs"

;; Implemented from paper

(defn ami?o
  [x]
  (let [y (sum-fak x)]
    (if (= y x)
      false
      (if (= x (sum-fak y))
        [x y]
        false))))

(defn eul21o
  [lim]
  (loop [i 1 res []]
    (if (= i lim)
      res
      (let [stop (last res)]
        (if (= i stop)
          (recur (inc i) res)
          (recur (inc i) (if (ami?o i) (concat res (ami?o i)) res)))))))
;;Problem 35

(defn rotate
  ;; return
  [x]
  (let [coll (num2seq x)]
    (if (= 1 (count coll))
      coll
      (for [i (range 1 (inc (count coll)))]
        (seq2num (take (count coll) (drop i (cycle coll))))))))


(defn eul35
  [lim]
  (loop [i 1 res []]
    (if (= i lim)
      res
      (if (or
            (some #(= i %) res)
            (not (prime? i)))
        (recur (inc i) res)
        (let [roti (rotate i)]
          (if (some #(not (prime? %)) roti)
            (recur (inc i) res)
            (recur (inc i) (concat res roti))))))))

;;"Elapsed time: 12624.563651 msecs"

;;Problem 44

(defn triangle
  ([] (triangle 1))
  ([n] (cons (/ (* n (+ n 1)) 2) (lazy-seq (triangle (inc n))))))

(defn pentagonal
  ([] (pentagonal 1))
  ([n] (cons (/ (* n (- (* 3 n) 1)) 2) (lazy-seq (pentagonal (inc n))))))



(defn penta [n] (/ (*' n (- (* 3 n) 1)) 2))



(defn penta? [n] (let [d (Math/sqrt (+ 1 (* 4 3 (* 2 n))))]
                   (if (== d (int d))
                     (let [i (/ (+ d 1) 6)]
                       (== i (int i)))
                     false)))

(defn eul44 [] (loop [i 2 res 0]
                 (let [p1 (penta 1)])))

(defn checkpenta [i] (let [p1 (penta i)]
                       (loop [j (dec i)]
                         (if (> 1 j)
                           0
                           (let [p2 (penta j)
                                 p3 (penta (dec j))
                                 d (Math/abs (- p2 p1))]
                             (if (and (= p1 (+ p2 p3))
                                      (penta? d))
                               p1
                               (recur (dec j))))))))

(defn hexagonal
  ([] (hexagonal 1))
  ([n] (cons (* n (- (* 2 n) 1)) (lazy-seq (hexagonal (inc n))))))

(defn pp
  ([] (pp 1 2))
  ([a b] (cons a (lazy-seq (pp b (+ a b))))))

(defn tri [n] (/ (* n (+ n 1)) 2))

(defn pen [n] (/ (* n (- (* 3 n) 1)) 2))

(defn hex [n] (* n (- (* 2 n) 1)))

(defn eul45 [start] (loop [it 286 ip 165 ih 143]
                      (let [t (tri it)
                            p (pen ip)
                            h (hex ih)]
                        (if (= t p h)
                          t
                          (if (< p t)
                            (recur it (inc ip) ih)
                            (if (< h t)
                              (recur it ip (inc ih))
                              (if (or (< t p) (< t h))
                                (recur (inc it) ip ih))))))))

;;Problem 74

(defn fak! [x] (reduce *' (range 1 (inc x))))

(defn sum-fak-dig [x] (sum (map fak! (num2seq x))))

(defn chain [x] )

;;Problem 89

(defn rr [st] (letfn [(roman? [ch] (cond
                                     (= \I ch) 1
                                     (= \V ch) 5
                                     (= \X ch) 10
                                     (= \L ch) 50
                                     (= \C ch) 100
                                     (= \D ch) 500
                                     (= \M ch) 1000
                                     :else 0))]
                (loop [coll (map roman? st) res 0]
                  (if (empty? coll)
                    res
                    (if (nil? (second coll))
                      (+ res (first coll))
                      (if (< (first coll) (second coll))
                        (recur (drop 2 coll) (+ res (- (second coll) (first coll))))
                        (recur (rest coll) (+ res (first coll)))))))))

(defn wr [i] (loop [x i res []]
               (cond
                 (>= x 1000) (recur (- x 1000) (conj res "M"))
                 (>= x 900) (recur (- x 900) (conj res "CM"))
                 (>= x 500) (recur (- x 500) (conj res "D"))
                 (>= x 400) (recur (- x 400) (conj res "CD"))
                 (>= x 100) (recur (- x 100) (conj res "C"))
                 (>= x 90) (recur (- x 90) (conj res "XC"))
                 (>= x 50) (recur (- x 50) (conj res "L"))
                 (>= x 40) (recur (- x 40) (conj res "XL"))
                 (>= x 10) (recur (- x 10) (conj res "X"))
                 (>= x 9) (recur (- x 9) (conj res "IX"))
                 (>= x 5) (recur (- x 5) (conj res "V"))
                 (>= x 4) (recur (- x 4) (conj res "IV"))
                 (>= x 1) (recur (- x 1) (conj res "I"))
                 :else (apply str res))))

;;Problem 22





;;Problem 53

(defn eul53 [n par] (loop [i 1 res 0]
                      (let [coll (pascal-tri i)]
                        (if (< n i)
                          res
                          (recur (inc i) (+ res (count (filter #(< par %) coll))))))))

;;Problem 63

(defn eul63 [] (loop [p 1 res 0]
                 (let [coll (filter #(= p (count (str %))) (map #(pow % p) (range 1 10)))]
                   (if (empty? coll)
                     res
                     (recur (inc p) (+ res (count coll)))))))

;; problem 315

(def sseg {:nil  [0 0 0 0 0 0 0]
           :1   [0 1 0 0 1 0 0]
           :2   [1 0 1 1 1 1 0]
           :3   [1 1 1 0 1 1 0]
           :4   [1 1 0 0 1 0 1]
           :5   [1 1 1 0 0 1 1]
           :6   [1 1 1 1 0 1 1]
           :7   [0 1 0 0 1 1 1]
           :8   [1 1 1 1 1 1 1]
           :9   [1 1 1 0 1 1 1]
           :0   [1 1 0 1 1 1 1]})

(defn defkey [v] (cond
                   (= 1 v) :1
                   (= 2 v) :2
                   (= 3 v) :3
                   (= 4 v) :4
                   (= 5 v) :5
                   (= 6 v) :6
                   (= 7 v) :7
                   (= 8 v) :8
                   (= 9 v) :9
                   (= 0 v) :0
                   :else :nil))

(defn count-trans [a b] (let [va (sseg (defkey a))
                              vb (sseg (defkey b))]
                          (count (filter true? (map (fn [ba bb]
                                                      (let [s (+ ba bb)]
                                                        (cond
                                                          (= 2 s) false
                                                          (= 1 s) true
                                                          (= 0 s) false)))
                                                    va vb)))))

(defn dig2dig
  ;;exam input [1 3 7] [nil 1 1] [nil nil 2]
  [v vres]
  (reduce + (map count-trans v vres)))

(defn dig2seq
  ;;123 (1 2 3)
  [x]
  (map #(- (int %) 48) (str x)))

(defn syncdig [ref x] (concat (take (- (count ref) (count x)) (repeat nil)) x))

(defn spsum [coll] (reduce + (filter #(not (nil? %)) coll)))

(defn genbootmax [n] (loop [vek (dig2seq n) res [(take (count vek) (repeat nil))]]
                       (if (= 1 (count (filter #(not (nil? %)) vek)))
                         (conj res vek (take (count vek) (repeat nil)))
                         (recur (syncdig vek (dig2seq (spsum vek)))
                                (conj res vek)))))

(defn genbootsam [n] (loop [vek (dig2seq n) res [(take (count vek) (repeat nil))]]
                       (if (= 1 (count (filter #(not (nil? %)) vek)))
                         (conj res vek (take (count vek) (repeat nil)))
                         (recur (syncdig vek (dig2seq (spsum vek)))
                                (conj res vek (take (count vek) (repeat nil)))))))

(defn caltrans [coll] (loop [[x & xs] coll res 0]
                        (if (empty? xs)
                          res
                          (recur xs (+ res (dig2dig x (first xs)))))))

;;


(defn tr
  ([] (tr 1))
  ([n] (cons (* n n) (lazy-seq (tr (+ 2 n))))))

(defn kell
  ([] (kell 8))
  ([n] (cons n (lazy-seq (kell (+ 8 n))))))

(defn eul28 [lim] (loop [x 3 coll 8 res 0]
                    (let [x1 (* x x) mun (quot coll 4)]
                      (if (< lim x)
                        res
                        (recur (+ x 2)
                               (+ coll 8)
                               (+ res (- (* 4 x1) (* 6 mun))))))))

;;Problen 58

(defn gen-spiral [n] (loop [i 3 z 1 nc 1 lp 1 ]
                       (if (< (/ lp nc) n)
                         i
                         (let [ncoll (- (* 4 i) 4) coll (range (+ z ncoll) (inc z) (- (quot ncoll 4)))]
                           (recur (+ i 2)
                                  (first coll)
                                  (+ 4 nc)
                                  (+ (count (filter prime? coll)) lp))))))

;;problem 92
(defn sq [n] (*' n n))

(defn sq-dig [n] (sum (map sq (num2seq n))))

(defn sq-chain? [n] (loop [x n]
                      (if (or (= 1 x)
                              (= 89 x))
                        x
                        (recur (sq-dig x)))))

(defn eul92 [lim] (loop [i 1 coll [] res 0]
                    (if (= lim i)
                      res
                      (let [x (sq-chain? i)]
                        (if (= 89 x)
                          (recur (inc i) (conj coll i) (inc res))
                          (recur (inc i) coll res))))))


;; Problem 24

(defn lex-perm [n coll] (loop [resn (dec n) c coll res []]
                          (let [f (fak! (dec (count c)))
                                nt (quot resn f)]
                            (if (= 1 (count c))
                              (concat res c)
                              (recur (rem resn f)
                                     (remove #{(nth c nt)} c)
                                     (conj res (nth c nt)))))))

;; Problem 145
(defn revnum [n] (seq2num (reverse (num2seq n))))

(defn last-dig [n] (- (int (last (str n))) 48))

(defn first-dig [n] (- (int (first (str n))) 48))

(defn rever? [n] (let [a (first-dig n)
                       b (last-dig n)]
                   (cond (zero? b) false
                         (even? (+ a b)) false
                         :else (every? odd? (num2seq (+ n (revnum n)))))))

(defn eul145 [lim] (loop [i 1 lib [] res 0]
                     (if (= i lim)
                       res
                       (recur (inc i)
                              lib
                              (if (rever? i) (inc res) res)))))

;; Problem 18

;;recusive sum

(defn sumq [[x & xs]] (if (empty? xs)
                        x
                        (+ x (sumq xs))))

(defn max* [[x & xs]]
  (if (empty? xs)
    x
    (let [temp (max* xs)]
      (if (> x temp)
        x
        temp))))


;; problem 18


;;"Elapsed time: 34.19313 msecs"
;;memoized



(defn eul18 [tree] (loop [x (first (reverse tree)) xs (rest (reverse tree))]
                     (if (empty? xs)
                       x
                       (let [temp (first xs)]
                         (recur (map #(let [a (get x (.indexOf xs %))
                                            b (get x (inc (.indexOf xs %)))]
                                       (if (> a b)
                                         (+ a %)
                                         (+ b %)))
                                     xs)
                                (rest xs))))))

(defn ssd? [coll] (let [d1 (get coll 1)
                        d2 (get coll 2)
                        d3 (get coll 3)]
                    (if (even? (seq2num [d1 d2 d3]))
                      false
                      (let [d4 (get coll 4)]
                        (if (= 0 (rem (seq2num [d2 d3 d4]) 3))
                          false
                          (let [d5 (get coll 5)]
                            (if (= 0 (rem (seq2num [d3 d4 d5]) 5))
                              false
                              (let [d6 (get coll 6)]
                                (if (= 0 (rem (seq2num [d4 d5 d6]) 7))
                                  false
                                  (let [d7 (get coll 7)]
                                    (if (= 0 (rem (seq2num [d5 d6 d7]) 11))
                                      false
                                      (let [d8 (get coll 8)]
                                        (if (= 0 (rem (seq2num [d6 d7 d8]) 13))
                                          false
                                          (let [d9 (get coll 9)]
                                            (if (= 0 (rem (seq2num [d7 d8 d9]) 17))
                                              false
                                              true)))))))))))))))


(defn eul43 [coll]
  (let [lim (fak! (count coll))]
    (loop [i 1 res 0]
      (if (> i 10000)
        res
        (let [x (vec (lex-perm i coll))]
          (recur (inc i) (if (ssd? x) (inc res) res)))))))

(defn eul48 [lim]
  (loop [i 1 res 0]
    (if (> i lim)
      res
      (let [x (rem (pow i i) 10000000000)]
        (recur (inc i)
               (+ res x))))))

;;Problem 102

(def tricoor (map #(clojure.string/split % #",") (clojure.string/split-lines (slurp "TriangleCoor.txt"))))

(def coor (mapv #(mapv (fn [x] (Integer/parseInt x)) %) tricoor))

(def tc (map #(partition 2 %) coor))

(defn triarea2
  [a b]
  (let [x1 (first a)
        x2 (first b)
        y1 (last a)
        y2 (last b)
        alas (Math/abs (- x1 x2))
        tinggi (Math/abs (- y1 y2))]
    (/ (* alas tinggi) 2)))

;;\displaystyle T = \frac{1}{2} | (x_A - x_c)(y_B-y_A) - (x_A - x_B)(y_C-y_A)|

(defn triarea3
  ;;return the area of three coordinate
  [coll]
  (let [a (first coll)
        b (second coll)
        c (last coll)
        lx (map first coll)
        ly (map last coll)
        xa (first a)
        xb (first b)
        xc (first c)
        ya (last a)
        yb (last b)
        yc (last c)]
    (/ (Math/abs (- (* (- xa xc) (- yb ya)) (* (- xa xb) (- yc ya)))) 2)))

(defn containori?
  [coll]
  (let [area1 (triarea3 coll)
        a (first coll)
        b (second coll)
        c (last coll)
        o '(0 0)
        t1 (triarea3 (list a b o))
        t2 (triarea3 (list a c o))
        t3 (triarea3 (list b c o))]
    (= area1 (+ t1 t2 t3))))


(defn quadran?
  ;; check return 1 2 3 or 4
  [[x y]]
  (cond (and (< 0 x) (< 0 y)) 1
        (and (> 0 x) (< 0 y)) 2
        (and (> 0 x) (> 0 y)) 3
        (and (< 0 x) (> 0 y)) 4
        :else 0))

(defn con-ori? [coll] (= 3 (count (filter #(not (zero? %)) (distinct coll)))))