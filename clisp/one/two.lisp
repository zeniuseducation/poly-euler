(defun div (a m )
  (declare (optimize (speed 3))
	   (fixnum a m))
  (truncate (/ a m)))

(defun frequencies (lsm)
  (let ((sls (sort lsm '<)))
    (labels ((helper (ls p i res1 res)
	       (if (null ls)
		   (cons (list p i) (cons res1 res))
		   (if (= p (first ls))
		       (helper (rest ls) p (1+ i) res1 res)
		       (helper (rest ls)
			       (first ls) 1
			       (list p i)
			       (cons res1 res))))))
      (butlast (helper (rest sls) (first sls) 1 nil nil)))))

(defun numcol (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((looper (i res)
	      (declare (fixnum i))
	      (if (< i 10)
		  (cons i res)
		  (looper (div i 10)
		     (cons  (rem i 10) res )))))
    (looper n nil)))

(defun range (i j k)
  (declare (optimize (speed 3))
	   (fixnum i j k))
  (if (< i j)
      (loop for m from i to j by k collect m)
      (loop for m from i downto j by k collect m)))

(defun take (n ls)
  (declare (optimize (speed 3))
	   (fixnum n))
  (if (= n 0) '() (cons (first ls) (take (- n 1) (rest ls)))))

(defun drop (n ls)
  (declare (optimize (speed 3))
	   (fixnum n))
  (if (= n 0) ls (drop (- n 1) (rest ls))))

(defun permutations (ls)
  (declare (optimize (speed 3)))
  (if (= 1 (length ls))
      (mapcar 'list ls)
      (loop for i in ls
	 append (loop for rs in (permutations (remove i ls))
		   collect (cons i rs)))))

(defun colnum (ls)
  (declare (optimize (speed 3)))
  (labels ((looper (ls res)
	      (if (null ls)
		  res
		  (looper (rest ls) (+ (* res 10) (first ls))))))
    (looper ls 0)))

(defun permute (n ls)
  (declare (optimize (speed 3))
	   (fixnum n))
  (if (= 1 n)
      (mapcar 'list ls)
      (mapcan #'(lambda (s)
		  (mapcar #'(lambda (x)
			      (cons s x))
			  (permute (- n 1) (remove s ls)))) ls)))


(defun factors (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((step (if (evenp n) 1 2)))
    (labels ((looper (i res)
		(declare (fixnum i))
		(if (>= (* i i) n)
		    (if (= (* i i) n)
			(append (take (/ (length res) 2) res)
				(list i)
				(drop (/ (length res) 2) res))
			res)
		    (if (= 0 (rem n i))
			(looper (+ i step)
			   (append (take (/ (length res) 2) res)
				   (list i (truncate (/ n i)))
				   (drop (/ (length res) 2) res)))
			(looper (+ i step) res)))))
      (if (evenp n) (looper 2 (list 1 n)) (looper 3 (list 1 n))))))


(defun pitas1 (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (time
   (first
    (sort
     (frequencies
      (loop for a from 3 to (/ lim 4) append
	   (loop for d in (if (evenp a)
			      (cons 1 (range 2 a 2))
			      (range 1 a 2))
	      for asqr = (* a a)
	      for b = (/ (- (/ asqr d) d) 2)
	      while (>= b a)
	      for c = (+ b d)
	      for peri = (+ a b c)
	      when (and (<= peri lim)
			(integerp b))
	      collect peri)))
     '> :key 'second))))

(defun pitas1b (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (time
   (first
    (sort
     (loop for a from 3 to (/ lim 4) append
	  (loop for d in (if (evenp a)
			     (cons 1 (range 2 a 2))
			     (range 1 a 2))
	     for asqr = (* a a)
	     for b = (/ (- (/ asqr d) d) 2)
	     while (>= b a)
	     for c = (+ b d)
	     for peri = (+ a b c)
	     when (and (<= peri lim)
		       (integerp b))
	     collect peri))
     '> :key 'second))))


(defun pitas2 (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (time
   (let ((refs (make-array (+ 1 lim) :initial-element 0))
	 (res 0)
	 (ires 0))
     (progn (loop for a from 3 to (/ lim 4) append
		 (loop for d from (let ((tmp (floor (/ (* a a) lim))))
				    (if (> tmp 0)
					(if (evenp a)
					    (if (evenp tmp) tmp (+ tmp 1))
					    (if (oddp tmp) tmp (+ tmp 1)))
					(if (evenp a) 2 1)))
		    to (ceiling (/ a 2)) by 2
		    for asqr = (* a a)
		    for b = (/ (- (/ asqr d) d) 2)
		    while (>= b a)
		    for c = (+ b d)
		    for peri = (+ a b c)
		    when (and (<= peri lim)
			      (integerp b))
		    collect (progn (let ((tmp (aref refs peri)))
				     (setf (aref refs peri) (+ tmp 1)))
				   peri)))
	    (progn (loop for i from 12 to lim by 2
		      for m = (aref refs i)
		      when (> m res)
		      do (progn (setf res m)
				(setf ires i)))
		   (list res ires))))))

(defun pair-factors (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((step (if (evenp n) 1 2)))
    (labels ((looper (i res)
		(declare (fixnum i))
		(if (> (* i i) n)
		    res
		    (if (= 0 (rem n i))
			(looper (+ i step)
			   (cons (list i (div n i)) res))
			(looper (+ i step) res)))))
      (looper (if (evenp n) 2 3) (list (list 1 n))))))

(defun pitas3 (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (time
   (sort
    (loop for b from 4 to lim by 2
       for al = (- b 1)
       for bl = (/ (- (* al al) 1) 2)
       for peril = (+ al bl bl 1)
       for res = (pair-factors b)
       append (loop for k in res
		 for m = (second k)
		 for n = (first k)
		 for a = (- (* m m) (* n n))
		 for c = (+ (* m m) (* n n))
		 for peri = (+ a b c)
		 while (<= peri lim)
		 when (not (= m n))
		 collect peri)) '<)))

(defun pitas4 (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (time
   (let ((refs (make-array (1+ lim) :initial-element 0 ))
	 (refsa (make-array (1+ lim) :initial-element '() )))
     (progn (loop for b from 2 to (/ lim 4)
	       for res = (pair-factors b)
	       do (loop for k in res
		     for m = (second k)
		     for n = (first k)
		     for a = (- (* m m) (* n n))
		     for c = (+ (* m m) (* n n))
		     for peri = (+ a (* 2 b) c)
		     while (<= peri lim)
		     when (and (= 1 (gcd m n)) )
		     do (let ((tmpa (aref refsa a)))
			  (if (member (* 2 b) tmpa)
			      nil
			      (progn
				(loop for idx from peri to lim by peri
				   do (let ((tmp (aref refs idx)))
					(setf (aref refs idx) (+ tmp 1))))
				(loop for idxa from 0 to (/ lim (* 4 a))
				   do (let ((tmpaa (aref refsa (* idxa a))))
					(setf (aref refsa (* idxa a))
					      (cons (* idxa (* 2 b)) tmpaa)))))))))
	    (let ((res 0)
		  (ires 0))
	      (progn (loop for i from 12 to lim by 2
			when (> (aref refs i) res)
			do (progn (setf res (aref refs i))
				  (setf ires i)))
		     (list ires res)))))))

(defun pitas5 (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (time
   (let ((refs (make-array (1+ lim) :initial-element 0 ))
	 (refsa (make-array (1+ lim) :initial-element '() )))
     (progn (loop for b from 2 to lim
	       for res = (if (oddp b)
			     (pair-factors b)
			     (pair-factors (/ b 2)))
	       do (if (oddp b)
		      (loop for k in res
			 for m = (second k)
			 for n = (first k)
			 for a = (/ (- (* m m) (* n n)) 2)
			 for c = (/ (+ (* m m) (* n n)) 2)
			 for peri = (+ a b c)
			 while (and (<= peri lim) (< a b))
			 when (and (> a 0) (= (gcd  m n) 1))
			 do (let ((tmpa (aref refsa a)))
			      (if (member b tmpa)
				  nil
				  (progn
				    (loop for idx from peri to lim by peri
				       do (let ((tmp (aref refs idx)))
					    (setf (aref refs idx) (+ tmp 1))))
				    (loop for idxa from 1 to (/ lim (* 2 a))
				       do (let ((tmpaa (aref refsa (* idxa a))))
					    (setf (aref refsa (* idxa a))
						  (cons (* idxa b) tmpaa))))))))
		      (loop for k in res
			 for m = (second k)
			 for n = (first k)
			 for a = (- (* m m) (* n n))
			 for c = (+ (* m m) (* n n))
			 for peri = (+ a (* 2 b) c)
			 while (and (<= peri lim) (< a (* 2 b)))
			 when (and (> a 0) (= (gcd  m n) 1))
			 do (let ((tmpa (aref refsa a)))
			      (if (member (* 2 b) tmpa)
				  nil
				  (progn
				    (loop for idx from peri to lim by peri
				       do (let ((tmp (aref refs idx)))
					    (setf (aref refs idx) (+ tmp 1))))
				    (loop for idxa from 1 to (/ lim (* 2 a))
				       do (let ((tmpaa (aref refsa (* idxa a))))
					    (setf (aref refsa (* idxa a))
						  (cons (* idxa (* 2 b)) tmpaa))))))))))
	    (let ((res 0)
		  (resi 0))
	      (loop for i from 12 to lim by 2
		 counting (= (aref refs i) 1) into counter
		 when (> (aref refs i) res)
		 do (progn (setf res (aref refs i))
			   (setf resi i))
		 finally (return (list counter resi res))))))))

(defun pitas (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (time
   (let ((refs (make-array (+ 1 lim) :initial-element 0)))
     (progn (loop for m from 2 to (/ lim 2)
	       do (loop for n from 1 to (- m 1)
		     for a = (- (* m m) (* n n))
		     for b = (* 2 m n)
		     for c = (+ (* m m) (* n n))
		     for peri = (+ a b c)
		     while (<= peri lim)
		     when (and (or (evenp m) (evenp n))
			       (= 1 (gcd m n)))
		     do (loop for idx from peri to lim by peri
			   do (let ((tmp (aref refs idx)))
				(setf (aref refs idx) (+ tmp 1))))))
	    (let ((res 0) (ires 0))
	      (loop for idx from 12 to lim by 2
		 for tmp = (aref refs idx)
		 when (> tmp res)
		 do (progn (setf res tmp)
			   (setf ires idx))
		 counting (= 1 tmp) into counter
		 finally (return (list counter ires res))))))))



(defun hexal? (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((res (/ (+ 1 (sqrt (+ 1.0d0 (* 8 n)))) 4)))
    (= (floor res) (ceiling res))))

(defun next-penxal (start)
  (declare (optimize (speed 3))
	   (fixnum start))
  (labels ((looper (i)
	      (declare (fixnum i))
	      (let ((pen (/ (* i (- (* 3 i) 1)) 2)))
		(if (<= pen start)
		    (looper (1+ i))
		    (if (hexal? pen)
			pen
			(looper (1+ i)))))))
    (time (looper 1))))

(defun penxal (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (labels ((looper (i res)
	      (declare (fixnum i))
	      (let ((pen (/ (* i (- (* 3 i) 1)) 2)))
		(if (> pen lim)
		    res
		    (if (hexal? pen)
			(looper (1+ i) (cons pen res))
			(looper (1+ i) res))))))
    (time (looper 1 nil))))


















