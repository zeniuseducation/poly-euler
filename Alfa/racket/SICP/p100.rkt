#lang racket

;; Racket

(define (sol1 lim)
  (- (+ (foldl + 0 (range 3 lim 3))
	(foldl + 0 (range 5 lim 5)))
     (foldl + 0 (range 15 lim 15))))

(define (sol2 lim (a 1) (b 0) (res 0))
  (if (> a lim)
      res
      (sol2 lim (+ a b) a (if (even? a) (+ a res) res))))

(define (odd-prime? p)
  (let ((lim (sqrt p)))
    (define (loop i)
      (if (> i lim)
	  true
	  (if (= 0 (remainder p i))
	      false
	      (loop (+ i 2)))))
    (loop 3)))

(define (take-while f xs (res '()))
  (let ((x (first xs)))
    (if (f x)
	(take-while f (rest xs) (cons x res))
	(reverse res))))

(define (drop-while f xs)
  (if (f (first xs))
      (drop-while f (rest xs))
      xs))

(define (next-prime-divides n p)
  (if (odd-prime? p)
      (if (= 0 (remainder n p))
	  p
	  (next-prime-divides n (+ p 2)))
      (next-prime-divides n (+ p 2))))

(define (sol3 target (p 3) (lprime 3))
  (if (= 1 target)
      lprime
      (let ((next-prime (next-prime-divides target p)))
	(sol3 (quotient target next-prime)
	      (+ p 2)
	      next-prime))))






