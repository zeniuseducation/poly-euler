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

;; Runs in 8ms
(defun sol21 (n)
  (deff n)
  (let* ((lim (* 3 n))
	 (llim (isqrt lim))
	 (faks (make-array (1+ lim) :initial-element 1)))
    (loop for i from 2 to llim
       do (let ((isqr (* i i)))
	    (setf (aref faks isqr) (+ (aref faks isqr) i))
	    (loop for j from (+ isqr i) to lim by i
	       do (setf (aref faks j) (+ (aref faks j) i (div j i))))))
    (loop for i from 1 to n
       for itmp = (aref faks i)
       when (and (not (= i itmp)) (= i (aref faks itmp)))
       summing i into sumi
       finally (return sumi))))




(defun abuns (lim)
  (deff lim)
  (let* ((llim (isqrt lim))
	 (faks (make-array (+ lim 1) :initial-element 1)))
    (loop for i from 2 to llim
       do (let ((isqr (* i i)))
	    (setf (aref faks isqr) (+ (aref faks isqr) i))
	    (loop for j from (+ isqr i) to lim by i
	       do (setf (aref faks j) (+ (aref faks j) i (div j i))))))
    (labels ((iter (i res)
	       (if (> i lim)
		   res
		   (if (> (aref faks i) i)
		       (iter (+ i 1) (cons i res))
		       (iter (+ i 1) res)))))
      (reverse (iter 1 nil)))))

;; Runs in 102ms
(defun sol23 (lim)
  (deff lim)
  (let* ((abunj (abuns lim))
	 (len (length abunj))
	 (abun (make-array len :initial-contents abunj))
	 (llim (/ lim 2))
	 (sums (make-array (+ lim 1) :initial-element nil)))
    (loop for i from 0 to (1- len)
       for itmp = (aref abun i)
       when (> itmp llim)
       return (- (/ (* lim (+ lim 1)) 2)
		 (sum (filter (fn (aref sums %)) (range 1 lim 1))))
       do (loop for j from i to (1- len)
	     for jtmp = (aref abun j)
	     for ijtmp = (+ itmp jtmp)
	     when (<= ijtmp lim)
	     do (setf (aref sums ijtmp) t)
	     when (> ijtmp lim)
	     return nil))))












