#lang racket

(require math)
(define rem remainder)
(define div quotient)
(define neg? negative?)
(define rd radians->degrees)
(define dr degrees->radians)
(define (f= x i)
  (<= (- i 0.01) x (+ i 0.01)))

(define (gdeg m)
  (rd (atan m)))

(define (grad point)
  (/ (* -4 (first point)) (second point)))

(define l list)

(define (lgrad p1 p2)
  (match-let ([(list x1 y1) p1]
              [(list x2 y2) p2])
    (let ((m (/ (- y2 y1) (- x2 x1)))) m)))

(define (next-grad m1 m2)
  (let* ((dm1 (gdeg m1))
         (dm2 (gdeg m2))
         (diff (- dm1 dm2)))
    (tan (dr (- dm2 diff)))))


