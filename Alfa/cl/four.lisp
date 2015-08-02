(load "clojure.lisp")

(defun sum-sieve (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (let* ((llim (isqrt lim))
	 (hlim (if (evenp llim) (1+ llim) (+ llim 2)))
	 (primes (make-array (+ lim 1) :initial-element true))
	 (res 2))
    (loop for i from 3 to llim by 2
       when (aref primes i)
       do (progn (loop for j from (* i i) to lim by (* i 2)
		    do (setf (aref primes j) false))
		 (setf res (+ res i))))
    (loop for i from hlim to lim by 2
       when (aref primes i)
       summing i into sumi
       finally (return (+ sumi res)))))

(defun sol5 (lim)
  (deff lim)
  (let ((faks (make-array (+ lim 1) :initial-contents (range 20)))
	(res 1))
    (loop for i from 2 to lim
       do (let ((p (aref faks i)))
	    (progn (loop for j from (* i 2) to lim by i
		      do (setf (aref faks j) (/ (aref faks j) p)))
		   (setf res (* res p))))
       finally (return res))))

(defun sol7 (tar)
  (deff tar)
  (let* ((lim (* tar 12))
	 (llim (ceiling (sqrt lim)))
	 (hlim (if (evenp llim) (+ llim 1) (+ llim 2)))
	 (primes (make-array lim :initial-element true))
	 (cur 1))
    (loop for i from 3 to llim by 2
       when (aref primes i)
       do (progn (loop for j from (* i i) to lim by (* 2 i)
		    do (setf (aref primes j) false))
		 (setf cur (1+ cur))))
    (loop for i from hlim to lim by 2
       when (aref primes i)
       do (setf cur (1+ cur))
       when (= cur tar)
       return i)))

(defun sol7b (tar)
  (deff tar)
  (let* ((lim (* tar 12))
	 (llim (ceiling (sqrt lim)))
	 (hlim (if (evenp llim) (+ llim 1) (+ llim 2)))
	 (primes (make-array lim :initial-element true))
	 (cur 1))
    (labels ((outer (i cur)
	       (deff i cur)
	       (if (> i llim)
		   cur
		   (if (aref primes i)
		       (progn (loop for j from (* i i) to lim by (* 2 i)
				 do (setf (aref primes j) false))
			      (outer (+ i 2) (1+ cur)))
		       (outer (+ i 2) cur)))))
      (progn (setf cur (outer 3 1))
	     (loop for i from hlim to lim by 2
		when (aref primes i)
		do (setf cur (1+ cur))
		when (= cur tar)
		return i)))))



