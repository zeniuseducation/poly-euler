(ns poly-euler.next.p82
  (:require
   [clojure.set :refer [union difference intersection]]))

(set! *unchecked-math* true)

(defn psqr?
  [n]
  (let [ck (Math/sqrt n)]
    (= (Math/floor ck) (Math/ceil ck))))

(defn sqr [x] (* x x))

(defn ^long area-triangles
  [^long lim]
  (loop [i (int 2) res 0]
    (if (>= (* 3 i) lim)
      res
      (let [a (inc i) b (dec i)
            p1 (quot (+ i i a) 2)
            p2 (quot (+ i i b) 2)
            asq (*' p1 (- p1 i) (- p1 i) (- p1 a))
            bsq (*' p2 (- p2 i) (- p2 i) (- p2 b))]
        (if (psqr? asq)
          (if (psqr? bsq)
            (recur (inc i) (+ res
                              (+ (* 2 i) a)
                              (+ (* 2 i) b)))
            (recur (inc i) (+ res (+ (* 2 i) a))))
          (if (psqr? bsq)
            (recur (inc i) (+ res (+ (* 2 i) b)))
            (recur (inc i) res)))))))

(defn ^long sol94
  [^long lim]
  (loop [i (int 2) res 0]
    (let [a (inc i) b (dec i)
          p1 (/ (+ i i a) 2)
          p2 (/ (+ i i b) 2)
          asq (*' p1 (- p1 i) (- p1 i) (- p1 a))
          bsq (*' p2 (- p2 i) (- p2 i) (- p2 b))]
      (if (> (* 2 p2) lim)
        res
        (if (and (<= (* 2 p1) lim) (psqr? asq))
          (if (psqr? bsq)
            (recur (inc i) (+ res
                              (+ (* 2 i) a)
                              (+ (* 2 i) b)))
            (recur (inc i) (+ res (+ (* 2 i) a))))
          (if (psqr? bsq)
            (recur (inc i) (+ res (+ (* 2 i) b)))
            (recur (inc i) res)))))))


(defn pitas
  [^long lim]
  (loop [i (int 2) res (long 0)]
    (if (> (dec (* 3 i)) lim)
      res
      (let [a (inc i) b (dec i)
            asq (- (sqr i) (sqr (/ a 2)))
            bsq (- (sqr i) (sqr (/ b 2)))]
        (if (and (<= (inc (* 3 i)) lim)
                 (psqr? asq))
          (if (psqr? bsq)
            (recur (inc i) (+ res
                              (inc (* 3 i))
                              (dec (* 3 i))))
            (recur (inc i) (+ res (inc (* 3 i)))))
          (if (psqr? bsq)
            (recur (inc i) (+ res (dec (* 3 i))))
            (recur (inc i) res)))))))


"518408346"


(defn ^long sol94b
  [^long lim]
  (->> (pitas lim)
       (map #(% 3))
       (reduce +)))




