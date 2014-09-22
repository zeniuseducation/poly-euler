;; This is an attempt to make some of clojure functions runs on SBCL


(defconstant true t)
(defconstant false nil)

(defun inc (x) (+ 1 x))
(defun dec (x) (- x 1))

(defun zero? (x) (zerop x))

(defun true? (x) (equal true x))

(defun nil? (x) (null x))

(defun empty? (x) (null x))

(defun quot (a m)
  (floor (/ a m)))

(defun filter (f ls)
  (remove-if-not f ls))

(defun sum (ls)
  "Returns the sum of all elements in ls"
  (apply '+ ls))

(defun product (ls)
  "Returns the product of all elements in ls"
  (apply '* ls))

(defun div? (a m)
  "Returns true if a is evenly-divisible by m"
  (zerop (rem a m)))

(defun square (x)
  (* x x))

(defun sqr (x) (* x x))

(defun range (&rest args)
  "Clojure range behaviour"
  (cond ((= 1 (length args))
	 (let* ((i (first args)))
	   (loop for x from 0 to (1- i) collect x)))
	((= 2 (length args))
	 (let* ((i (first args)) (j (second args)))
	   (loop for x from i to (1- j) collect x)))
	((= 3 (length args))
	 (let* ((i (first args)) (j (second args)) (k (third args)))
	   (if (>= i j)
	       (loop for x from i downto j by (abs k) collect x)
	       (loop for x from i to j by k collect x))))))

(defun prime-helper (p i)
  (cond ((> (* i i) p) t)
	((zerop (rem p i)) nil)
	(t (prime-helper p (+ 2 i)))))

(defun prime? (p)
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

(defun factors (n)
  (factors-helper n 1 '()))

(defun sum-factors (n)
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
  (count-factors-helper n 1 0))

(defun lcm-list (ls res) 
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

(defun take (n ls)
  (cond ((>= n (length ls)) ls)
	((= n 1) (list (first ls)))
	(:else (append (list (first ls))
		       (take (1- n) (rest ls))))))

;; Some performance test

(defun next-prime (x)
  "Returns the next positive prime number larger than x"
  (cond ((= 2 x) 3)
	((evenp x) (next-prime (1+ x)))
	((prime? (+ 2 x)) (+ 2 x))
	(:else (next-prime (+ 2 x)))))

(defun prime-list-helper (n i cur res)
  (if (= n i)
      (cons cur res)
      (prime-list-helper n (+ 1 i) (next-prime cur) (cons cur res))))

(defun prime-list (n)
  "Returns the n first positive integers"
  (prime-list-helper n 1 2 '()))


(defun suma-prima (n)
  "Returns the sum of n first positive prime numbers"
  (sum (prime-list n)))

(defun primes-under (n)
  "Returns the sum of all primes under n"
  (loop for i from 2 to n when (prime? i) collect i))

"Elapsed time 3secs for n=100,000"

(defun sum-primes-helper (n i cur res)
  "Helper function using res as sum accumulator, and i as the current prime"
  (if (<= n i)
      (+ cur res)
      (sum-primes-helper n (+ i 1) (next-prime i) (+ res cur))))

(defun sum-primes (n)
  "Returns the sum of n first positive prime numbers"
  (sum-primes-helper n 1 2 0))

"Elapsed time 1sec for n=100,000"

(defun primes-under-helper (n i res)
  (if (<= n i)
      res
      (primes-under-helper n (next-prime i) (cons i res))))

(defun primes-under-1 (n)
  "Returns all positive primes less than n"
  (cond ((<= n 2) '())
	(:else (reverse (primes-under-helper n 2 nil)))))

(defun numcol-helper (n res)
  (if (< n 10)
      (cons n res)
      (numcol-helper (quot n 10) (cons (rem n 10) res))))

(defun numcol (n)
  "Returns the list of digits in a number n"
  (numcol-helper n nil))

(defun colnum-helper (ls res)
  (if (nil? ls)
      res
      (colnum-helper (rest ls) (+ (* 10 res) (first ls)))))

(defun colnum (ls)
  "Construct a number based on digits in a list"
  (colnum-helper ls 0))

(defun manual-pascal-row (res)
  (cons 1
	(reverse (cons 1
		       (mapcar #'(lambda (x y) (+ x y))
			       res (rest res))))))

(defun manual-pascal-helper (n i res)
  (if (= n i)
      (manual-pascal-row res)
      (manual-pascal-helper n (1+ i) (manual-pascal-row res))))

(defun manual-pascal (n)
  "Returns the n-th row of pascal triangle"
  (cond ((= n 1) '(1))
	((= n 2) '(1 1))
	(:else (manual-pascal-helper n 2 '(1 1)))))

(defun palin? (n)
  (let ((tmp (numcol n)))
    (equal tmp (reverse tmp))))












