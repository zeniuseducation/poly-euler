
(load "clojure.lisp")


(defun qd-helper (d y)
  (let ((xsqr (+ 1 (* d y y))))
    (if (psqr? xsqr)
	(list (sqrt xsqr) y)
	(qd-helper d (+ 1 y)))))

(defun quad-diop (d)
  (cons d (qd-helper d 1)))

(defun diops (i j)
  (remove-if 'psquare? (range i j)))

(defun sol66a (n d maxi)
  (cond ((psqr? d)
	 (sol66a n (+ 1 d) maxi))
	((> d n)
	 maxi)
	(:else (let ((tmp (quad-diop d)))
		 (if (> (second tmp) (second maxi))
		     (sol66a n (+ 1 d) tmp)
		     (sol66a n (+ 1 d) maxi))))))

(defun euler66 (n)
  (time (sol66a n 1 '(0 0 0))))

(defun solved? (d y)
  (list (sqrt (+ 1 (* d y y)))
	(1+ (* d y y)) d y))

;; PROBLEM 108 FOR 110

;; PROBLEM 125

(defun squares-inner (n i sumn lim res)
  (let ((tsqr (+ sumn (sqr i))))
    (if (>= tsqr lim)
	res
	(squares-inner n
		       (inc i)
		       tsqr
		       lim
		       (if (palin? tsqr)
			   (cons tsqr res)
			   res)))))

(defun squares-outer (n lim res)
  (let ((tmp (sqr (dec n))))
    (if (>= tmp lim)
	(sum (remove-duplicates res))
	(squares-outer (inc n)
		       lim
		       (append res (squares-inner n n tmp lim  nil))))))

(defun sol125 (lim)
  (time (squares-outer 2 lim nil)))


;; PROBLEM NO 131
;; n^3 + n^2p = m^3 -> p = (m^3 - n^3)/ n^2
;; m3/n2 - n = p => (n+d)^3/n2 - n = p
;; (3d^2/n + 3d + d^3/n^2)  = p
;; a^3.b^3 / n^2 - n = p => n^2 = a^3

(defun find-primes (ls i n lim res)
  (let* ((itmp (nth i ls)) (ntmp (nth (dec i) ls)))
    (if (> (- itmp ntmp) lim)
	res
	(let ((nb (if (< n 0) 0 (nth n ls))))
	  (if (or (< n 0) (> (- itmp nb) lim))
	      (find-primes ls (inc i) (- i 2) lim res)
	      (if (prime? (- itmp nb))
		  (find-primes ls (inc i) i lim (cons (- itmp nb) res))
		  (find-primes ls (inc i) i lim res)))))))

(defun cubes (i j lim res)
  (let ((ic (cube i)) (jc (cube j)))
    (if (> (- jc ic) lim)
	(cons jc res)
	(cubes (inc i) (inc j) lim (cons jc res)))))

(defun sol131 (lim)
  (time (find-primes (reverse (cubes 0 1 lim nil)) 1 0 lim nil)))

;; PROBLEM NO 134

(defun digc (p1 p2)
  (first (loop for i in '(1 3 5 7 9)
	    when (= (rem p1 10)
		    (rem (* p2 i) 10))
	    collect i)))

(defun pair134 (p1 p2 d)
  (let* ((tmp (numcol (* d p2)))
	 (nc (numcol p1))
	 (pp (drop (- (length tmp) (length nc)) tmp)))
    (if (equal pp nc)
	(* d p2)
	(pair134 p1 p2 (+ 10 d)))))

(defun fpairs (p1 p2 lim res i)
  (if (> p1 lim)
      res
      (let ((d (digc p1 p2)))
	(progn (if (zerop (rem i 1000)) (print i))
	       (fpairs p2 (next-prime p2) lim (+ (pair134 p1 p2 d) res) (inc i))))))

(defun sol134 (lim)
  (time (fpairs 5 7 lim 0 0)))   

;; problem no 113

(defun dis-integrate (ls)
  "Returns the discrete integration of polynom ls"
  (let* ((tmp (mapcar #'(lambda (x)
			  (let* ((power (inc (second x)))
				 (coef (* (first x) (/ 1 power))))
			    (list coef power)))
		      ls))
	 (new-coef (- 1 (sum (mapcar 'first tmp)))))
    (reverse (cons (list new-coef 1)
		   (reverse tmp)))))

(defun rec-integrate (pl n)
  "Returns the integration process for a polynom up to n times"
  (first (last (iterate 'dis-integrate pl
			#'(lambda (x) (> (length x) n))))))

(defun apply-poly (n pl)
  "Apply n into polynom pl"
  (sum (mapcar #'(lambda (x) (* (first x)
			   (expt n (second x))))
	       pl)))

(defun non-bouncy (n)
  "Returns the number of n digits non-bouncy numbers"
  (* 2 (sum (mapcar #'(lambda (x) (apply-poly 9 (rec-integrate '((1 1)) x)))
		    (range 2 (inc n))))))





