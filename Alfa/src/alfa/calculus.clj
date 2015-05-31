(ns alfa.calculus
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all]))

(defn sqrt
  "Just a wrapper for java thing"
  [x]
  (Math/sqrt x))

(defn line
  "Returns the line equation given a gradient and a point, or two points"
  [grad-or-point [x y]]
  (if (vector? grad-or-point)
    ;; if it's a vector then it's a point, otherwise it's a gradient
    (let [[xm ym] grad-or-point
          gradient (/ (- ym y) (- xm x))]
      [gradient (- y (* gradient x))])
    [grad-or-point (- y (* grad-or-point x))]))

(defn distance
  "Cartesian distance between two points"
  [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (sqrt (+ (* dx dx) (* dy dy)))))


(defn tansec-line
  "Given a point in the curve, returns the tangent or secant line"
  [[x y] whichone?]
  (let [gradient (if (= whichone? :tan)
                   (- (/ (* 4 x) y))
                   (/ y (* 4 x)))]
    [gradient (- y (* gradient x))]))


(defn intersect
  "Given two lines, return the intersection point"
  [[m1 c1] [m2 c2]]
  [(/ (- (- c1 c2)) (- m1 m2)) (+ (* m1 x) c1)])

(defn impact-point
  "Given a line equation and a point, return the intersection point with the curve"
  [[m c] [x1 y1]]
  (let [xf-1 (/ (+ (* -2 m c) (sqrt (- (* 4 m m c c)
                                       (* 4 (+ 4 (* m m)) (- (* c c) 100)))))
                (+ 8 (* 2 m m)))
        xf-2 (/ (- (* -2 m c) (sqrt (- (* 4 m m c c)
                                       (* 4 (+ 4 (* m m)) (- (* c c) 100)))))
                (+ 8 (* 2 m m)))
        yf-1 ()]))

(defn next-point
  "Given two points, return the next point of 'impact'"
  [[x1 y1] [x2 y2]]
  (let [tanline (tansec-line [x2 y2] :tan)
        secline (tansec-line [x2 y2] :sec)
        midpoint-line (line (first secline) [x1 y1])
        [x-mid y-mid] (intersect midpoint-line secline)
        [x-ref y-ref] [(+ x-mid (- x-mid x1)) (+ y-mid (- y-mid y1))]
        refline (line [x2 y2] [x-ref y-ref])]))





