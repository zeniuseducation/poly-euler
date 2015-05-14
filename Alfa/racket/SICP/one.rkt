#lang racket

;; Implementing Newtonian method for finding square-root

(define (square x)
  (* x x))

(define (sol1-3 a b c)
  (foldl + 0 (map (lambda (x) (square x))
		  (remove (min a b c) (list a b c)))))

(define (my-sqrt n)
  (define (good-enough? m)
    (< (abs (- (square m) n)) 0.00000001))
  (define (improve guess)
    (/ (+ guess (/ n guess)) 2.0))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (cond ((< n 0) "Can't do this!")
	((= n 0) 0)
	(#t (sqrt-iter n))))

(define (fibo n (a 1) (b 1) (i 1))
  (if (= i n) a (fibo n (+ a b) a (+ i 1))))

(define (nth xs i)
  (if (or (< i 0) (empty? xs))
      "index overflow"
      (if (= 0 i)
	  (first xs)
	  (nth (rest xs) (- i 1)))))
