(ns euler.prob151-200)

;; PROBLEM 173/174

(load-file "math.clj")

(defn sqr [x] (* x x))

(defn add2 [x] (+ 2 x))
(defn min2 [x] (- x 2))

(defn laminas
  [n t]
  (->> (- itmp jtmp)
       (for [j (range (min2 i) 0 -2)
             :let [jtmp (sqr j)]
             :while (<= (- itmp jtmp) n)])
       (for [i (iterate add2 t)
             :let [itmp (sqr i)]
             :while (<= (- itmp (sqr (min2 i))) n)])
       (apply concat)
       (count)))

(defn lamina [n] (time (+ (laminas n 3) (laminas n 4))))

(defn lamina_2
  [lim]
  (->> tiles
       (for [j (range (add2 i) (add2 limx) 2)
             :let [tiles (- (sqr j) (sqr i))]
             :while (<= tiles lim)])
       (for [i (range 1 (inc limx))])
       (let [limx (/ lim 4)])
       (map count)
       (sum)
       (time)))

(defn lamina3
  [lim]
  (time
   (loop [x 1 y (add2 x) res 0]
     (let [tiles (- (sqr y) (sqr x))]
       (if (> tiles lim)
         (if (> x (/ lim 4))
           res
           (recur (inc x) (+ x 3) res))
         (recur x (add2 y) (inc res)))))))

(defn lamina-group
  [lim]
  (loop [x 1 y (add2 x) res []]
    (let [tiles (- (sqr y) (sqr x))]
      (if (> tiles lim)
        (if (> x (/ lim 4))
          res
          (recur (inc x) (+ x 3) res))
        (recur x (add2 y) (conj res tiles))))))

(defn sol174
  [lim]
  (->> (lamina-group lim)
       frequencies
       (group-by val)
       (map #(vector (key %) (count (val %))))
       (filter #(<= (first %) 10))
       (map second)
       (sum)
       (time)))







