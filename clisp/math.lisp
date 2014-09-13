;; This is an attempt to make some of clojure functions runs on SBCL


(defconstant true t)
(defconstant false nil)

(defun zero? (x) (zerop x))

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

(defun prime-helper (p i lim)
  (cond ((>= i lim) t)
	((zerop (rem p i)) nil)
	(t (prime-helper p (+ 2 i) lim))))

(defun prime? (p)
  (cond ((<= p 20) (if (member p '(2 3 5 7 11 13 17 19)) t nil))
	((evenp p) nil)
	(t (prime-helper p 3 (sqrt p)))))

(defun factors-helper (n i res lim)
  (cond ((> i lim) res)
	((zerop (rem n i)) (factors-helper n
					   (1+ i)
					   (if (= i (quot n i))
					       (cons i res)
					       (cons i (cons (/ n i) res)))
					   lim))
	(t (factors-helper n (1+ i) res lim))))

(defun factors (n)
  (factors-helper n 2 '() (1+ (sqrt n))))

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

(defun next-prime (x)
  "Returns the next positive prime number larger than x"
  (cond ((= 2 x) 3)
	((evenp x) (next-prime (1+ x)))
	((prime? (+ 2 x)) (+ 2 x))
	(:else (next-prime (+ 2 x)))))

;; Some performance test

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
