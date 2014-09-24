(ns euler.prob51-100)

(load-file "math.clj")

;; Dont know ini nomer berapa

(defn sqr [x] (* x x))

(defn a->bc
  "Consider a^2 + b^2 = c^2, this fn returns the value of a+b+c from a
  given a, or nil if it's impossible to construct integer-value b and
  c from a given a."
  [a lim]
  (let [asqr (sqr a)]
    (loop [b (inc a)]
      (if (or (> (* 2 b) asqr)
              (> (+ a b) (* 2 (/ lim 3))))
        nil
        (if-let [sol (loop [x 1]
                       (let [tmp (* x (+ x (* 2 b)))]
                         (if (or (> tmp asqr)
                                 (> (+ a b (+ b x)) lim))
                           nil
                           (if (= tmp asqr)
                             (+ a b (+ b x))
                             (recur (inc x))))))]
          sol
          (recur (inc b)))))))

(defn lazyfor
  [a lim]
  (let [asqr (sqr a)]
    (for [b (iterate inc a)
          :while (or (<= (* 2 b) asqr)
                     (<= (+ a b) (* 2 (/ lim 3))))
          :let [sol (loop [x 1]
                      (let [tmp (* x (+ x (* 2 b)))]
                        (if (or (> tmp asqr)
                                (> (+ a b (+ b x)) lim))
                          nil
                          (if (= tmp asqr)
                            (+ a b (+ b x))
                            (recur (inc x))))))]] sol)))

(defn soulmate
  [lim]
  (->> (inc (quot lim 3))
       (range 3)
       (mapcat #(lazyfor % lim))
       (remove nil?)
       (group-by identity)
       (filter #(= 1 (count (second %))))
       count))

(defn foulmate
  [lim]
  (->> (inc (quot lim 3))
       (range 3)
       (map #(a->bc % lim))
       (remove nil?)
       frequencies
       (filter #(= 1 (val %))) 
       count))



;; Problem 61

(defn figurates
  [ftype n]
  (cond (= ftype 3) (/ (* n (inc n)) 2)
        (= ftype 4) (sqr n)
        (= ftype 5) (/ (* n (dec (* 3 n))) 2)
        (= ftype 6) (* n (dec (* 2 n)))
        (= ftype 7) (/ (* n (- (* 5 n) 3)) 2)
        (= ftype 8) (* n (- (* 3 n) 2))
        :else 0))

(defn look1
  [n digit]
  (->> (iterate inc 1)
       (map #(figurates n %))
       (drop-while #(< % (expt 10 (dec digit))))
       (take-while #(< % (expt 10 digit)))))

(defn look2
  [digit]
  (map #(look1 % digit) (range 3 9)))


;; Problem 66

;; x^2 = 1 + dy2
;; for d <= 1000

(defn psqr?
  [n]
  (let [x (Math/sqrt n)]
    (== (bigint x) x)))

(defn qdiop
  [d]
  (loop [y 1]
    (let [xsqr (inc (*' d y y))]
      (if (psqr? xsqr)
        [d (bigint (Math/sqrt xsqr)) y]
        (recur (inc y))))))

(defn look66-1
  [i j]
  (time (loop [d i [dr xr yr] [0 0 0]]
          (if (> d j)
            [dr xr yr]
            (if (psqr? d)
              (recur (inc d) [dr xr yr])
              (recur (inc d)
                     (let [[dt x y] (qdiop d)]
                       (if (> x xr)
                         [dt x y]
                         [dr xr yr]))))))))

;; Problem 100

(defn look100-1
  [start]
  (for [i (iterate inc start)
        :let [b (-' (bigint (* i (/ 1 (Math/sqrt 2)))) 100)
              tmp (/ (*' b (dec b))
                     (*' i (dec i)))]
        :when (== tmp 1/2)] [i b tmp]))

(defn look100-2
  [start]
  (loop [tot start]
    (if-let [sol (loop [b (-' (bigint (* tot (/ 1 (Math/sqrt 2)))) 2)]
                   (let [tmp (/ (*' b (dec b)) (*' tot (dec tot)))]
                     (if (> tmp 1/2)
                       nil
                       (if (== 1/2 tmp)
                         [tot b]
                         (recur (inc b))))))]
      sol
      (recur (inc tot)))))

(defn sol100
  [n]
  (->> (iterate #(let [tmp (/ (first %) (nth % 2))
                       sol (look100-2 (dec (bigint (* tmp (first %)))))]
                   (conj sol (first %)))
                [120 0 21])
       (drop-while #(< (first %) (expt 10 n)))
       (take 1)))

(defn look100-3
  [n]
  (iterate #(look100-2 (dec (bigint (* 5.8 (first %)))))
           [120 0 0]))

(comment )


