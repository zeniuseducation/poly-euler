
(load "math.lisp")

(defun primes-in-range (p1 p2 sp1 sp2)
  (- (+ (sum (range (+ p1 sp1) sp2 p1))
	(sum (range (- sp2 p2) sp1 (- p2))))
     (* 2 p1 p2)))

(defun outer-collector (p1 p2 lim res)
  (let ((sp1 (sqr p1))
	(sp2 (sqr p2)))
    (if (>= sp2 lim)
	(+ res (- (+ (sum (range (+ p1 sp1) (inc lim) p1))
		     (sum (remove #'(lambda (x) (> x lim))
				  (range (- sp2 p2) sp1 (- p2)))))
		  (if (> (* p1 p2) lim) 0 (* 2 p1 p2))))
	(outer-collector p2
			 (next-prime p2)
			 lim
			 (+ res (primes-in-range p1 p2 sp1 sp2))))))

(defun sol234 (lim)
  (time (outer-collector 2 3 lim 0)))
















