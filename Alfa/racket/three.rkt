#lang racket

(require math)

(define (sol179 lim)
  (define (loop (i 2) (res 0))
    (if (> i lim)
        res 
        (if (= (length (divisors i))
               (length (divisors (+ i 1))))
            (loop (+ i 1) (+ res 1))
            (loop (+ i 1) res))))
  (loop))

(define modi (expt 10 9))

(define tar (expt 10 12))

(define (log10 a)
  (/ (log a) (log 10)))

(define (count-dig a)
  (ceiling (log10 a)))

(define (sol458 tar modi n)
  (let* ((hasil (modular-expt n tar modi))
         (perms (modular-expt (factorial n)
                              (quotient tar n)
                              modi)))
    (modulo (- hasil (modulo (* (factorial n)
                                (modular-expt n (- tar n) modi))
                             modi)) modi)))

(define (fibo lim (a 1) (b 0) (i 2))
  (if (> a lim) i (fibo lim (+ a b) a (+ 1 i))))

(define (sol lim)
  (let* ((primes (make-vector (+ lim 1) true))
         (refs (apply vector (range (+ lim 1))))
         (llim (ceiling (sqrt lim))))
    (define (loopi i res)
      (define (loopj j)
        (if (> j lim)
            false
            (begin (when (<= i llim)
                     (vector-set! primes j false))
                   (vector-set! refs j 
                                (quotient (* (vector-ref refs j) (- i 1)) i))
                   (loopj (+ j i)))))
      (if (> i lim)
          res
          (if (vector-ref primes i)
              (begin (loopj (* i 2))
                     (loopi (+ i 1)
                            (+ res (- i 1))))
              (loopi (+ i 1)
                     (+ res (vector-ref refs i))))))
    (loopi 2 0)))

(define (mytot n)
  (let ((pfs (prime-divisors n)))
    (quotient (* n (foldl * 1 (map (lambda(x) (- x 1)) pfs)))
              (foldl * 1 pfs))))

(define (sola lim)
  (let ((primes (make-vector (+ lim 1) true))
        (llim 1000))
    (define (loopi i res)
      (define (loopj j)
        (if (> j lim)
            false 
            (begin (vector-set! primes j false)
                   (loopj (+ j i)))))
      (if (> i lim)
          res
          (if (vector-ref primes i)
              (if (<= i llim)
                  (begin (loopj (* i i))
                         (loopi (+ i 1)
                                (+ res (- i 1))))
                  (loopi (+ i 1) (+ res (- i 1))))
              (loopi (+ i 1) (+ res (mytot i))))))
    (loopi 2 0)))

(define (sol10 lim)
  (let ((refs (make-vector (+ lim 1) true))
        (llim (ceiling (sqrt lim))))
    (define (loopi i res)
      (define (loopj j)
        (if (> j lim)
            false 
            (begin (vector-set! refs j false)
                   (loopj (+ j (* 2 i))))))
      (if (> i lim) 
          res
          (if (vector-ref refs i)
              (if (<= i llim)
                  (begin (loopj (* i i))
                         (loopi (+ i 2) (+ i res)))
                  (loopi (+ i 2) (+ i res)))
              (loopi (+ i 2) res))))
    (loopi 3 2)))

(define (cube n)
  (eval-poly (map (lambda (x) (if (even? x) 1 -1)) (range 0 11)) n))

(define (soltol n)
  (define (fn a b)
    (let ((ai (+ 1 a))
	  (bi (+ 1 b)))
      (expt ai b)))
  (let* ((matcol (list->matrix n 1 (map cube (range 1 (+ n 1)))))
	 (matm (build-matrix n n fn)))
    (array->list (matrix-solve matm matcol))))

(define (eval-poly xs n)
  (foldl + 0 (map (lambda (x y) (* x (expt n y)))
		  xs (range (length xs)))))

(define (soltar lim)
  (foldl + 0 (map (lambda (x y) (eval-poly x y))
		  (map soltol (range 1 lim))
		  (range 2 (+ lim 1)))))

(define (abc-hits lim)
  (let ((lima (- (quotient lim 2) 1))
	(res 0))
    (define (loopi a)
      (define (loopj b)
	(let ((c (+ a b)))
	  (if (>= c lim)
	      false
	      (if (pairwise-coprime? a b c)
		  (if (< (foldl * 1 (prime-divisors (* a b c))) c)
		      (begin (set! res (+ res 1))
			     (loopj (+ b 1)))
		      (loopj (+ b 1)))
		  (loopj (+ b 1))))))
      (if (> a lima)
	  res
	  (begin (loopj (+ a 1))
		 (loopi (+ a 1)))))
    (loopi 1)))








