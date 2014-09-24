
(load "math.lisp")

;; Problem 234



(defun primes-in-range (p1 p2 sp1 sp2)
  (- (+ (sum (range (+ p1 sp1) sp2 p1))
	(sum (range (- sp2 p2) sp1 (- p2))))
     (* 2 p1 p2)))


(defun outer-collector (p1 p2 lim res)
  (let ((sp1 (sqr p1))
	(sp2 (sqr p2)))
    (if (> sp2 lim)
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


;; PROBLEM 243


(defconstant base-primes (primes-under 1000))


(defun add-pmultiples (p lim res)
  "Returns the set union of res and a list of multiples of p up to lim"
  (union res (range p lim p)))

(defun add-primes (ls lim res)
  (if (empty? ls)
      res
      (add-primes (rest ls) lim (add-pmultiples (first ls) lim res))))

(defun resil (n lim)
  "Returns the first number that has resillience less than lim"
  (let* ((praw (take n base-primes))
	 (num (product praw))
	 (nres (/ (- num (inc (length (add-primes praw num nil))))
		  (dec num))))
    (progn (print num)
	   (if (< nres lim)
	       (list num nres)
	       (resil (inc n) lim)))))

(defun sol243 (lim)
  (time (resil 2 lim)))
