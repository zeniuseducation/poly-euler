(ns alfa.special.p144
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

(defn atan [l] (Math/atan l))
(defn tan [l] (Math/tan l))
(def PI Math/PI)

(declare f= reflection-grad impact-point grad next-grad next-impact)

(defn sol144
  ([p1 p2]
   (sol144 p1 p2 0))
  ([[x1 y1 :as p1] [x2 y2 :as p2] res]
   (if (and (<= (- y2 0.01) 10 (+ y2 00.1)) (f= x2 0))
     res
     (recur p2 (next-impact p1 p2) (+ res 1)))))

(defn next-impact
  [p1 p2]
  (let [m1 (reflection-grad p1 p2)
        m2 (grad p2)]
    (impact-point (next-grad m1 m2) p2)))

(defn impact-point
  [m [x y]]
  (let [c (- y (* m x))
        bb (* 2 m c)
        aa (+ (* m m) 4)
        cc (- (* c c) 100)
        det (sqrt (- (* bb bb) (* 4 aa cc)))
        x1 (/ (+ (- bb) det) (* 2 aa))
        x2 (/ (- (- bb) det) (* 2 aa))
        y1 (+ (* m x1) c)
        y2 (+ (* m x2) c)]
    (if (and (f= x1 x) (f= y1 y))
      [x2 y2]
      [x1 y1])))

(defn f=
  ([x i] (f= x i 0.01))
  ([x i lim] (<= (- i lim) x (+ i lim))))

(defn grad [[x y]] (/ (* -4 x) y))

(defn reflection-grad
  [[x1 y1] [x2 y2]]
  (/ (- y2 y1) (- x2 x1)))

(defn next-grad
  [m1 m2]
  (let [am1 (atan m1)
        am2 (atan m2)
        diff (- am1 am2)]
    (tan (+ (- PI (* 2 diff)) am1))))