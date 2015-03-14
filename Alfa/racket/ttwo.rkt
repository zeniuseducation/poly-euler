#lang typed/racket

(require math)


(define: (get-result (m : Integer))
  : Integer
  (remainder (+ m
		(modular-inverse (- m 2) m)
		(modular-inverse (* (- m 2) (- m 3)) m)
		(modular-inverse (* (- m 2) (- m 3) (- m 4)) m)) m))

(define: (sol (lim : Integer))
  : Integer 
  (define: (loop (i : Integer) (res : Integer))
    : Integer
    (if (> i lim)
	res
	(if (prime? i)
	    (loop (+ i 2) (+ res (get-result i)))
	    (loop (+ i 2) res))))
  (loop 7 4))

(define: (sol381 (lim : Integer))
  : Integer
  (let: ((refs : (Vectorof Boolean) (make-vector (+ lim 1) true)))
    (define: (loopj (j : Integer) (step : Integer))
      : Boolean
      (if (> j lim)
	  true
	  (begin (vector-set! refs j false)
		 (loopj (+ j step) step))))
    (define: (sieve (i : Integer) (res : Integer))
      : Integer
      (if (> i lim)
	  res
	  (if (vector-ref refs i)
	      (begin (when (<= (* i i) lim)
		       (loopj (* i i) (* i 2)))
		     (sieve (+ i 2)
			    (if (>= i 7)
				(+ res (get-result i))
				res)))
	      (sieve (+ i 2) res))))
    (sieve 3 4)))

