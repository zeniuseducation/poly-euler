#lang typed/racket

(define: (prime? (p : Integer))
  : Boolean
  (define (looper (i : Integer))
    : Boolean
    (if (> (* i i) p)
	true
	(if (= 0 (remainder p i))
	    false
	    (looper (+ i 2)))))
  (looper 3))

(define: (sum-primes (lim : Integer))
  : Integer
  (define: (looper (i : Integer) (res : Integer))
    : Integer
    (if (> i lim)
	res
	(looper (+ i 2)
		(if (prime? i)
		    (+ res i)
		    res))))
  (looper 3 2))



