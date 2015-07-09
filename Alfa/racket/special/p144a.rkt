#lang racket

(require math)
(define rem remainder)
(define div quotient)
(define neg? negative?)
(define rd radians->degrees)
(define dr degrees->radians)
(define l list)

(define (p144 p1 p2 (res 1))
  "The main function for solving 144, it iterates over points of impact"
  (match-let (((list x2 y2) p2)
              ((list x1 y1) p1))
    (if (and (<= (- y2 0.01) 10 (+ y2 00.1)) (f= x2 0))
        res 
        (p144 p2 (next-impact p1 p2) (+ res 1)))))

(define (next-impact p1 p2)
  "Find the next impact point by calculating p1 p2"
  (let ((m1 (reflection-grad p1 p2))
        (m2 (grad p2)))
    (impact-point (next-grad m1 m2) p2)))

(define (impact-point m p)
  "Returns the next impact point by using the gradient and the starting point"
  (match-let* (((list x y) p)
               (c (- y (* m x)))
               (bb (* 2 m c))
               (aa (+ (* m m) 4))
               (cc (- (* c c) 100))
               (det (sqrt (- (* bb bb) (* 4 aa cc))))
               (x1 (/ (+ (- bb) det) (* 2 aa)))
               (x2 (/ (- (- bb) det) (* 2 aa)))
               (y1 (+ (* m x1) c))
               (y2 (+ (* m x2) c)))
    (if (and (f= x1 x) (f= y1 y))
        (l x2 y2)
        (l x1 y1))))
               

(define (f= x i (lim 0.01))
  "A work around for floating point equality"
  (<= (- i lim) x (+ i lim)))

(define (grad point)
  (/ (* -4 (first point)) (second point)))

(define (reflection-grad p1 p2)
  "Returns the gradien of the reflection line"
  (match-let ([(list x1 y1) p1]
              [(list x2 y2) p2])
    (/ (- y2 y1) (- x2 x1))))

(define (next-grad m1 m2)
  "Find the gradien of the reflection line, m1 is trajectory gradien, m2 is slope of t-line"
  (let* ((am1 (atan m1))
         (am2 (atan m2))
         (diff (- am1 am2)))
    (tan (+ (- pi (* 2 diff)) am1))))


