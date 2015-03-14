(defconstant target (expt 10 999))

(defun fibo (a b i lim)
  (if (> a lim) i (fibo (+ a b) a (1+ i) lim)))

(defun prime? (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((looper (i)
	      (declare (fixnum i))
	      (if (> (* i i) n)
		  t
		  (if (= 0 (rem n i))
		      nil
		      (looper (+ i 2))))))
    (looper 3)))

(defun sum-primes (lim)
  (declare (optimize (speed 3))
	   (fixnum lim ))
  (labels ((looper (i res)
	      (declare (fixnum i res))
	      (if (> i lim)
		  res
		  (looper (+ i 2)
		     (if (prime? i) (+ res i) res)))))
    (looper 3 2)))

(defun sum-sieve (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (let* ((refs (make-array (+ lim 1) :initial-element t))
	 (llim (ceiling (sqrt lim))))
    (labels ((loopj (j step)
		(when (<= j lim)		  
		  (progn (setf (aref refs j) nil)
			 (loopj (+ j step) step))))
	     (loopi (i res)
		(declare (fixnum i res))
		(cond ((> i lim) res)
		      ((aref refs i) (if (< i llim)
					 (progn (loopj (* i i) (* i 2))
						(loopi (+ i 2) (+ i res)))
					 (loopi (+ i 2) (+ i res))))
		      (t (loopi (+ i 2) res)))))
      (loopi 3 2))))

(defun idem (x)
  (declare (optimize (speed 3) (safety 0)) (fixnum x))
  (labels ((loopi (i)
	      (if (= i (rem (* i i) x))
		  i
		  (loopi (- i 1)))))
    (loopi (- x 1))))

(defun sol (lim)
  (declare (optimize (speed 3) (safety 0)) (fixnum lim))
  (loop for i from 1 to lim
     summing (idem i) into sum
     finally (return sum)))

