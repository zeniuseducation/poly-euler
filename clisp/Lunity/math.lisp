(load "clojure.lisp")

(defun prime-helper (p i)
  (cond ((> (* i i) p) t)
	((zerop (rem p i)) nil)
	(t (prime-helper p (+ 2 i)))))

(defun prime? (p)
  "Prime checking function"
  (cond ((<= p 20) (if (member p '(2 3 5 7 11 13 17 19)) t nil))
	((evenp p) nil)
	(t (prime-helper p 3))))

(defun factors-helper (n i res)
  (cond ((> (* i i) n)
	 res)
	((zerop (rem n i))
	 (factors-helper n
			 (1+ i)
			 (if (= i (quot n i))
			     (cons i res)
			     (cons i (cons (/ n i) res)))))
	(t (factors-helper n (1+ i) res))))

(defun factors-help (n i res)
  (cond ((> (* i i) n)
	 res)
	((zerop (rem n i))
	 (factors-help n
		       (1+ i)
		       (if (= i (quot n i))
			   (let* ((lres (length res)))
			     (append (subseq res 0 (/ lres 2))
				     (cons i (subseq res (/ lres 2)))))
			   (let* ((lres (length res)))
			     (append (subseq res 0 (/ lres 2))
				     (append (list i (/ n i)) (subseq res (/ lres 2))))))))
	(t (factors-help n (1+ i) res))))

(defun factors (n)
  "Returns the positive integer factors of n"
  (factors-helper n 1 '()))

(defun sorted-factors (n)
  "Returns the positive integer factors of n"
  (sort (factors-helper n 1 '()) '<))

(defun sum-factors (n)
  "Returns the sum of n positive integer factors"
  (- (sum (factors n)) n))

(defun count-factors-helper (n i res)
  (cond ((> (* i i) n)
	 res)
	((zerop (rem n i))
	 (factors-helper n
			 (1+ i)
			 (if (= i (quot n i))
			     (inc res)
			     (+ 2 res))))
	(t (factors-helper n (1+ i) res))))

(defun count-factors (n)
  "Returns the number of positive integer factors of n"
  (count-factors-helper n 1 0))

(defun lcm-list (ls res)
  "Returns raw materials for lcm"
  (let ((a (first ls))
	(xs (rest ls)))
    (if (null xs)
	(cons a res)
	(if (some #'(lambda (x) (zerop (rem x a))) xs)
	    (lcm-list (mapcar #'(lambda (x) (if (zerop (rem x a))
					   (/ x a)
					   x))
			      xs)
		      (if (prime? a) (cons a res) res))
	    (lcm-list xs (cons a res))))))



(defun next-prime (x)
  "Returns the next positive prime number larger than x"
  (cond ((= 2 x) 3)
	((evenp x) (next-prime (1+ x)))
	((prime? (+ 2 x)) (+ 2 x))
	(:else (next-prime (+ 2 x)))))

(defun prev-prime (x)
  "Returns the next positive prime number less than x"
  (cond ((<= x 2) nil)
	((= x 3) 2)
	((evenp x) (prev-prime (1- x)))
	((prime? (- x 2)) (- x 2))
	(:else (prev-prime (- x 2)))))

(defun prime-list-helper (n i cur res)
  (if (= n i)
      (cons cur res)
      (prime-list-helper n
			 (+ 1 i)
			 (next-prime cur)
			 (cons cur res))))


(defun prime-list (n)
  "Returns the n first positive integers"
  (prime-list-helper n 1 2 '()))

(defun suma-prima (lim)
  "Returns the sum of all primes less than lim"
  (labels ((helper (cur res)
	     (if (>= cur lim)
		 res
		 (helper (next-prime cur)
			 (+ res cur)))))
    (helper 2 0)))

(defun primes-under (n)
  "Returns the sum of all primes under n"
  (loop for i from 2 to n when (prime? i) collect i))

"Elapsed time 3secs for n=100,000"

(defun sum-primes (n)
  "Returns the sum of n first positive prime numbers"
  (labels ((sum-primes-helper (n i cur res)
	     (if (<= n i)
		 (+ cur res)
		 (sum-primes-helper n
				    (+ i 1)
				    (next-prime i)
				    (+ res cur)))))
    (sum-primes-helper n 1 2 0)))

"Elapsed time 1sec for n=100,000"

(defun primes-under-helper (n i res)
  (if (<= n i)
      res
      (primes-under-helper n (next-prime i) (cons i res))))

(defun primes-under-1 (n)
  "Returns all positive primes less than n"
  (cond ((<= n 2) '())
	(:else (reverse (primes-under-helper n 2 nil)))))

(defun numcol (n)
  "Returns the list of digits in a number n"
  (labels ((numcol-helper (n res)
	     (if (< n 10)
		 (cons n res)
		 (numcol-helper (quot n 10) (cons (rem n 10) res)))))
    (numcol-helper n nil)))

(defun colnum (ls)
  "Construct a number based on digits in a list"
  (labels ((colnum-helper (ls res)
	     (if (nil? ls)
		 res
		 (colnum-helper (rest ls)
				(+ (* 10 res) (first ls))))))
    (colnum-helper ls 0)))

(defun pascal (n)
  "Returns the n-th row of pascal triangle"
  (cond ((= n 1) '(1))
	((= n 2) '(1 1))
	(:else (labels ((mph (n i res)
			  (labels ((mpr (res)
				     (cons 1
					   (reverse
					    (cons 1
						  (mapcar #'(lambda (x y) (+ x y))
							  res (rest res)))))))
			    (if (= n i)
				(mpr res)
				(mph n (1+ i) (mpr res))))))
		 (mph n 2 '(1 1))))))

(defun palin? (n)
  "Accepts an integer n and returns a list of its digits"
  (let ((tmp (numcol n)))
    (equal tmp (reverse tmp))))

(defun permute (ls)
  "Returns all possible permutations of ls"
  (if (= 1 (length ls))
      (mapcar 'list ls)
      (loop for i in ls
	 append (loop for rs in (permute (remove i ls))
		   collect (cons i rs)))))

(defun combine (ls n)
  "Takes n combinations of ls"
  (if (= 0 n)
      '(())
      (loop for i in ls
	    for j from 1 to (length ls)
	 append (loop for rs in (combine (drop j ls)
					 (dec n))
		   collect (cons i rs)))))

(defun iterate (fn i gn)
  "Returns non-lazy iterate while (gn i) is false"
  (if (funcall gn i)
      nil
      (cons i (iterate fn (funcall fn i) gn))))

(defun pfactors (n)
  "Returns all prime factors of n"
  (labels ((phelpers (p1 p2 res)
	     (if (prime? p2)
		 (cons p2 res)
		 (if (div? p2 p1)
		     (phelpers 2 (div p2 p1) (cons p1 res))
		     (phelpers (next-prime p1) p2 res)))))
    (reverse (phelpers 2 n nil))))

(defun pfacts (n)
  "Returns all prime factors of n"
  (labels ((divs (p1 n)
	     (if (zero? (rem n p1))
		 (list p1 (div n p1))
		 (divs (next-prime p1) n)))
	   (helper (n res)
	     (if (prime? n)
		 (cons n res)
		 (let ((result (divs 2 n)))
		   (helper (second result)
			   (cons (first result) res))))))
    (helper n nil)))

;; Dont use it, it's slow
(defun prima-lista1 (lim)
  "Returns the positive primes less than lim, lim should be greater than 10"
  (labels
      ((helper (prm n res)
	 (if (> n lim)
	     res
	     (if (empty? prm)
		 (helper (takelim (ceiling (sqrt (+ n 2))) res)
			 (+ n 2)
			 (append res (list n)))
		 (if (div? n (first prm))
		     (helper (takelim (ceiling (sqrt (+ n 2))) res)
			     (+ n 2)
			     res)
		     (helper (rest prm) n res))))))
    (cons 2 (helper '(3 5 7) 11 '(3 5 7)))))

;; Dont use it, it's slow
(defun prima-lista (lim)
  "Sieves less than lim"
  (labels
      ((hprime? (res p)
	 (if (> (sqr (first res)) p)
	     true
	     (if (div? p (first res))
		 false
		 (hprime? (rest res) p))))
       (helper (n res)
	 (if (> n lim)
	     res
	     (if (hprime? res n)
		 (helper (+ n 2) (append res (list n)))
		 (helper (+ n 2) res)))))
    (cons 2 (helper 11 '(3 5 7)))))
