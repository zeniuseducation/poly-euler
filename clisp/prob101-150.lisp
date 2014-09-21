(load "math.lisp")

(defun psqr-helper (n i)
  (cond ((> (* i i) n)
	 false)
	((= (* i i) n)
	 true)
	(:else (psqr-helper n (+ 1 i)))))

(defun psqr? (n)
  (psqr-helper n 1))

(defun psquare? (n)
  (= (floor (sqrt n)) (ceiling (sqrt n))))

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


<<<<<<< HEAD

;; problem 125
=======
;; PROBLEM 125
>>>>>>> origin/master

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
