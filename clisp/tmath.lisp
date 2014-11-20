(defun prime? (p)
  (declare (optimize speed) (fixnum p))
  (let ((lim (ceiling (sqrt p))))
    (declare (fixnum lim))
    (labels ((helper (i)
	       (declare (fixnum i))
	       (if (> i lim)
		   t
		   (if (= 0 (rem p i))
		       nil
		       (helper (+ i 2))))))
      (helper 3))))

(defun true-prime? (p)
  (declare (optimize speed) (fixnum p))
  (if (= 2 p)
      t
      (if (evenp p)
	  nil
	  (let ((lim (ceiling (sqrt p))))
	    (labels ((helper (i)
		       (declare (fixnum i))
		       (if (> i lim)
			   t
			   (if (= 0 (rem p i))
			       nil
			       (helper (+ i 2))))))
	      (helper 3))))))

(defun next-prime (p)
  (declare (optimize speed) (fixnum p))
  (if (= p 2)
      3
      (labels ((helper (i)
		 (declare (fixnum i))
		 (if (prime? i)
		     i
		     (helper (+ i 2)))))
	(helper (+ p 2)))))

(defun pfactors (p)
  (declare (optimize speed) (fixnum p))
  (labels ((helper (i n lasti res)
	     (declare (fixnum i n lasti))
	     (if (prime? n)
		 (if (= n lasti)
		     res
		     (cons n res))
		 (if (zerop (rem n i))
		     (helper 2 (/ n i) i (cons i res))
		     (helper (next-prime i) n lasti res)))))
    (helper 2 p 2 nil)))

(defun sum-primes (lim)
  (declare (optimize speed) (fixnum lim))
  (labels ((helper (i res)
	     (declare (fixnum i) (fixnum res))
	     (if (> i lim)
		 res
		 (if (prime? i)
		     (helper (+ i 2) (+ i res))
		     (helper (+ i 2) res)))))
    (helper 7 10)))

(defparameter limits (expt 10 999))

(defun fibo (lim)
  (declare (optimize speed))
  (labels ((helper (i j idx)
	     (declare (optimize speed))
	     (if (> i lim)
		 idx
		 (helper (+ i j) i (+ 1 idx)))))
    (helper 1 1 1)))

(defun nth-prime (n)
  "Returns the nth terms of positive primes"
  (declare (optimize speed) (fixnum n))
  (labels ((helper (i idx)
	     (declare (optimize speed)
		      (fixnum i idx))
	     (if (= idx n)
		 i
		 (helper (next-prime i) (+ 1 idx)))))
    (helper 2 1)))

(defun count-factors (n)
  (declare (optimize speed) (fixnum n))
  (let ((lim (ceiling (sqrt n))))
    (labels ((helper-even (i res)
	       (declare (optimize speed) (fixnum i res))
	       (if (> i lim)
		   res
		   (if (zerop (rem n i))
		       (let ((divs (/ n i)))
			 (if (= i divs)
			     (+ 1 res)
			     (helper-even (+ i 1) (+ 2 res))))
		       (helper-even (+ i 1) res))))
	     (helper-odd (i res)
	       (declare (optimize speed) (fixnum i res))
	       (if (> i lim)
		   res
		   (if (zerop (rem n i))
		       (let ((divs (/ n i)))
			 (if (= i divs)
			     (+ 1 res)
			     (helper-odd (+ i 2) (+ 2 res))))
		       (helper-odd (+ i 2) res)))))
      (if (oddp n)
	  (helper-odd 3 2)
	  (helper-even 2 2)))))

(defun sum-pdivs (n)
  (declare (optimize speed) (fixnum n))
  (let ((lim (ceiling (sqrt n))))
    (labels ((helper-even (i res)
	       (declare (optimize speed) (fixnum i res))
	       (if (> i lim)
		   res
		   (if (zerop (rem n i))
		       (let ((divs (/ n i)))
			 (if (= i divs)
			     (+ i res)
			     (helper-even (+ i 1) (+ i divs res))))
		       (helper-even (+ i 1) res))))
	     (helper-odd (i res)
	       (declare (optimize speed) (fixnum i res))
	       (if (> i lim)
		   res
		   (if (zerop (rem n i))
		       (let ((divs (/ n i)))
			 (if (= i divs)
			     (+ i res)
			     (helper-odd (+ i 2) (+ i divs res))))
		       (helper-odd (+ i 2) res)))))
      (if (oddp n)
	  (helper-odd 3 1)
	  (helper-even 2 1)))))

(defun sum-amic (lim)
  (declare (optimize speed) (fixnum lim))
  (labels ((helper (i res)
	     (declare (optimize speed) (fixnum i res))
	     (if (> i lim)
		 res
		 (let ((amic (sum-pdivs i)))
		   (if (= i amic)
		       (helper (+ 1 i) res)
		       (let ((div-amic (sum-pdivs amic)))
			 (if (= i div-amic)
			     (helper (+ 1 i) (+ i res))
			     (helper (+ 1 i) res))))))))
    (helper 2 0)))

(defun first-triangle-having-lim-factors (n lim)
  (declare (optimize speed) (fixnum n lim))
  (let* ((triangle (/ (* n (+ 1 n)) 2))
	 (factors (count-factors triangle)))
    (if (>= factors lim)
	(list n triangle)
	(first-triangle-having-lim-factors (+ 1 n) lim))))

(defun collatz (i)
  (declare (optimize speed) (fixnum i))
  (if (= 1 i)
      1
      (+ 1 (if (evenp i)
	       (collatz (/ i 2))
	       (collatz (+ 1 (* 3 i)))))))

(defun max-collatz-under-lim (starting lim)
  (declare (optimize speed) (fixnum starting lim))
  (labels ((helper (i res lres)
	     (declare (optimize speed)
		      (fixnum i res lres))
	     (if (> i lim)
		 res
		 (let ((colls (collatz i)))
		   (if (> colls lres)
		       (helper (+ 2 i) i colls)
		       (helper (+ 2 i) res lres))))))
    (helper starting 1 1)))










