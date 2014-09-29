;; This is an attempt to make some of clojure functions runs on SBCL

(defconstant true t)
(defconstant false nil)

(defun inc (x) (+ 1 x))
(defun dec (x) (- x 1))

(defun zero? (x) (zerop x))

(defun true? (x) (equal true x))

(defun false? (x) (not x))

(defun nil? (x) (null x))

(defun empty? (x) (null x))

(defun quot (a m)
  "Integer division, div"
  (floor (/ a m)))

(defun div (a m)
  "Integer division"
  (if (< a m) 0 (1+ (div (- a m) m))))

(defun filter (f ls)
  "Remove-if-not f ls"
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

(defun psqr? (n)
  (multiple-value-bind (x y) (truncate (sqrt n))
    (zerop y)))

(defun sqr (x) (* x x))

(defun cube (x) (* x x x))

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
	       (loop for x from i downto (inc j) by (abs k) collect x)
	       (loop for x from i to (dec j) by k collect x))))))

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

(defun factors (n)
  "Returns the positive integer factors of n"
  (factors-helper n 1 '()))

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

(defun take (n ls)
  "Returns a list containing n first elements of ls"
  (if (= n 0)
      '()
      (if (empty? ls)
	  ls
	  (cons (first ls) (take (dec n) (rest ls))))))

(defun take-while (fn ls)
  "Returns the elements of ls starting from first while (fn elmt) is true"
  (if (empty? ls)
      ls
      (if (not (funcall fn (first ls)))
	  nil
	  (cons (first ls) (take-while fn (rest ls))))))

(defun drop (n col)
  "Drop n first elements in col"
  (if (= n 0)
      col
      (drop (dec n) (rest col))))

;; Some performance test

(defun drop-while (fn ls)
  "Returns the elements of ls starting from first while (fn elmt) is true"
  (if (empty? ls)
      ls
      (if (funcall fn (first ls))
	  (drop-while fn (rest ls))
	  ls)))

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

(defun sum-primes (n)
  "Returns the sum of n first positive prime numbers"
  (labels ((sum-primes-helper (n i cur res)
	     (if (<= n i)
		 (+ cur res)
		 (sum-primes-helper n (+ i 1) (next-prime i) (+ res cur)))))
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
					   (reverse (cons 1
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
  "Returns non-lazy iterate while (gn i) is true"
  (if (not (funcall gn i))
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

(defun every? (fn ls)
  "Returns true if every element in ls satisfies fn"
  (if (empty? ls)
      true
      (if (not (funcall fn (first ls)))
	  false
	  (every? fn (rest ls)))))

(defun some? (fn ls)
  "Returns true if at least one  element in ls satisfies fn"
  (if (empty? ls)
      false
      (if (funcall fn (first ls))
	  true
	  (every? fn (rest ls)))))

(defun partial (fn &rest args)
  "Returns a curried version of fn"
  (lambda (&rest xs) (apply fn (append args xs))))

(defun cmap-helper (fn res args)
  (if (some? 'empty? args)
      res
      (cmap-helper fn
		   (cons (apply fn (mapcar 'first args)) res)
		   (mapcar 'rest args))))

(defun cmap (fn &rest args)
  "Clojure's map behaviour"
  (if (= 1 (length args))
      (mapcar fn (first args))
      (reverse (cmap-helper fn nil args))))

(defun comp-helper (ls)
  (if (= 1 (length ls))
      (lambda (x) (funcall (first ls) x))
      (lambda (x) (funcall (comp-helper (rest ls))
		      (funcall (first ls) x)))))

(defun comp (&rest args)
  "Clojure's comp with standard clisp behaviour (you need to call it with funcall)"
  (comp-helper (reverse args)))

(defun juxt-helper (ls x)
  (if (empty? ls)
      nil
      (cons (funcall (first ls) x)
	    (juxt-helper (rest ls) x))))

(defun juxt (&rest ls)
  "Clojure's juxt with clisp behaviour"
  (lambda (x) (juxt-helper ls x)))












