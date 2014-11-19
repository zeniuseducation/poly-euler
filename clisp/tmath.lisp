

(defun prime? (p)
  (declare (fixnum p))
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

(defun sum-primes (lim)
  (declare (fixnum lim))
  (labels ((helper (i res)
	     (declare (fixnum i) (fixnum res))
	     (if (> i lim)
		 res
		 (if (prime? i)
		     (helper (+ i 2) (+ i res))
		     (helper (+ i 2) res)))))
    (helper 7 10)))

(defun fibo (lim)
  (declare (optimize speed))
  (labels ((helper (i j idx)
	     (declare (optimize speed))
	     (if (> i lim)
		 idx
		 (helper (+ i j) i (+ 1 idx)))))
    (helper 1 1 1)))


