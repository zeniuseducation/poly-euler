#lang racket

(require math)

(require plot)

(define (sol211 lim)
  (foldl + 0 (filter perfect-square (map (lambda (x) (divisor-sum x 2))
					 (range 1 lim)))))

(define (sol211a lim)
  (foldl + 0 (filter (lambda (x) (perfect-square (divisor-sum x 2)))
		     (range 1 lim))))

(define (get-result m)
  (remainder (+ m
		(modular-inverse (- m 2) m)
		(modular-inverse (* (- m 2) (- m 3)) m)
		(modular-inverse (* (- m 2) (- m 3) (- m 4)) m)) m))

(define (sol lim)
  (let* ((refs (make-vector (+ lim 1) true))
	 (llim (ceiling (sqrt lim))))
    (define (loopj j step)
      (if (> j lim)
	  null
	  (begin (vector-set! refs j false)
		 (loopj (+ j step) step))))
    (define (sieve i res)
      (if (> i lim)
	  res
	  (if (vector-ref refs i)
	      (begin (when (<= i llim)
		       (loopj (* i i) (* i 2)))
		     (sieve (+ i 2)
			    (if (>= i 7)
				(+ res (get-result i))
				res)))
	      (sieve (+ i 2) res))))
    (sieve 3 4)))

(define (sol451 start lim)
  (define (loopi i res)
    (define (loopj j)
      (if (coprime? i j)
	  (if (= j (modular-inverse j i))
	      j
	      (loopj (- j 1)))
	  (loopj (- j 1))))
    (if (> i lim)
	res
	(loopi (+ i 1) (+ res (loopj (- i 2))))))
  (loopi start 0))

(define (loopji i)
  (define (loopj j)
    (if (coprime? i j)
	(if (= (modular-inverse j i) j)
	    j
	    (loopj (- j 1)))
	(loopj (- j 1))))
  (loopj (- i 2)))

(define (sol407 st end)
  (define (loopi i res)
    (define (loopj j)
      (let* ((g (gcd i j))
	     (ig (quotient i g))
	     (jg (quotient j g)))
	(if (= (modular-inverse jg ig) jg)
	    j
	    (loopj (- j 1)))))
    (if (> i end)
	res
	(loopi (+ i 1)
	       (+ res (loopj (- i 2))))))
  (loopi st 0))

(define (sol401 lim modi)
  (define (loopi i res)
    (if (> i lim)
	res
	(loopi (+ i 1)
	       (remainder (+ res (divisor-sum i 2)) modi))))
  (loopi 1 0))

(define (pascal lim)
  (map (lambda (x)
         (map (lambda (k)
                (let ((num (remainder (binomial x k) 5)))
                  num))
              (range (+ 1 x))))
       (range (+ 1 lim))))

(define (cpascal lim base)
  (map (lambda (x) (remainder x base))
       (map (lambda (x) (binomial (- lim 1) x))
	    (range lim))))

(define (log10 a)
  (/ (log a) (log 10)))

(define (repeat-d lim)
  (define (check x (diva 1000) (res 0) (xs '[]))
    (let ([t1 (remainder diva x)])
      (if (member t1 xs)
	  res
	  (check x (* 10 t1) (+ 1 res) (cons t1 xs)))))
  (define (loopi i res)
    (if (< i (second res))
	(first res)
	(let ([tmp (check i)])
	  (if (> tmp (second res))
	      (loopi (prev-prime i) (list i tmp))
	      (loopi (prev-prime i) res)))))
  (if (prime? lim)
      (loopi lim (list lim 0))
      (repeat-d (prev-prime lim))))

