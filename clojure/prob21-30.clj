(ns euler.prob21-30)

(load-file "math.clj")
;; NOTE, for this solution, it will cause stack-overflow since it uses
;; pure recursion without clojure's loop which is tail-recursion

(defn expt
  [a m]
  (if (zero? m) 1 (*' a (expt a (dec m)))))

(defn euler25
  [lim [a b] i]
  (if (>= a lim)
    i
    (euler25 lim [(+' a b) a] (inc i))))

;; poly-euler.prob25> (time (euler25 (expt 10 999) [1 1] 2))
;; "Elapsed time: 6.218 msecs"
"This one is actually faster 1.8msecs"

;; This is the TCO version

(defn euler25a
  [lim]
  (loop [[a b] [1 1] i 2]
    (if (>= a lim)
      i
      (recur [(+' a b) a]
             (inc i)))))

;; poly-euler.prob25> (time (euler25a (expt 10 999)))
;; "Elapsed time: 2.491 msecs"


;; Problem 21

(defn sum-prop-divs
  "It returns the sum of all proper divisors of n"
  [n]
  (- (sum (factors n)) n))

(defn amicable
  "It returns n if n is an amicable number or nil if otherwise"
  [n]
  (let [pair (sum-prop-divs n)]
    (if (and (not= n pair) (= n (sum-prop-divs pair))) n)))

(defn sol21
  "Returns the sum of all amicable numbers less than lim"
  [lim]
  (time (sum (keep amicable (range 1 lim)))))

"Elapsed time 80-90msecs"

;; PROBLEM NO 24

(defn selector
  [n]
  (->> (range (expt 10 (dec n)) (expt 10 n))
       (map numcol)
       (map distinct)
       (filter #(= n (count %)))
       (map colnum)
       sort
       (take 100)))

(defn zero-selector
  [n]
  (->> (range (expt 10 (- n 2)) (expt 10 (dec n)))
       (map numcol)
       (map #(remove #{0} %) )
       (map distinct)
       (filter #(= (dec n) (count %)))
       (map colnum)
       sort
       (map #(str 0 %))
       (take 100)))

(defn sol24
  [n]
  (time (concat (zero-selector n) (selector n))))

;; This one is very very slow but beautiful

(defn sol24
  [n]
  (loop [x n dig 10 res [] raw (range 10)]
    (if (= 0 dig)
      res
      (let [fak (product (range 1 dig))]
        (recur (rem x fak)
               (dec dig)
               (conj res (nth raw (quot x fak)))
               (remove #(= % (nth raw (quot x fak))) raw))))))
