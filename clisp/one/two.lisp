(defun div (a m )
  (declare (optimize (speed 3)))
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



(defun iterate (fn i gn)
  "Returns non-lazy iterate while (gn i) is false"
  (if (funcall gn i)
      nil
      (cons i (iterate fn (funcall fn i) gn))))

(defun dig-unique? (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((num (numcol n)))
    (equal num (remove-duplicates num))))

(defun snumcol (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((looper (i res)
	      (declare (fixnum i))
	      (if (< i 10)
		  (let ((num (cons i res)))
		    (cond ((= 2 (length num)) (cons 0 num))
			  ((= 1 (length num)) (append '(0 0) num))
			  (:else num)))
		  (looper (div i 10)
		     (cons  (rem i 10) res )))))
    (looper n nil)))

(defun divpandig2 ()
  (declare (optimize (speed 3)))
  (let ((refs '(13 11 7 5 3 2))
	(digs (range 0 9 1)))
    (labels ((looper (i cls)
		(declare (fixnum i))
		(if (> i 5)
		    (let ((num (set-difference digs cls)))
		      (list (append num cls)))
		    (let* ((p (nth i refs))
			   (raw1 (remove-if-not
				  #'(lambda (x) (dig-unique? x))
				  (range p 999 p)))
			   (raw2 (remove-if-not
				  #'(lambda (x) (and (not (member (first x) cls))
						(equal (rest x) (take 2 cls))))
				  (mapcar 'snumcol raw1))))
		      (mapcan
		       #'(lambda (x) (looper (+ i 1)
				   (cons (first x) cls)))
		       raw2)))))
      (time
       (reduce '+
	       (mapcar 'colnum
		       (mapcan #'(lambda (x) (looper 0 x))
			       (mapcar 'snumcol
				       (remove-if-not
					#'(lambda (x) (dig-unique? x))
					(range 17 999 17))))))))))

(defun divpandig ()
  (declare (optimize (speed 3)))
  (let ((digs (range 0 9 1))
	(raw '(13 11 7 5 3 2)))
    (labels ((looper (i cls rdigs)
		(declare (optimize (speed 3))
			 (fixnum i))
		(if (> i 5)
		    (list (append (set-difference digs cls) cls))
		    (let* ((p (nth i raw))
			   (raw1 (mapcar #'(lambda (x) (cons x (take 2 cls))) rdigs))
			   (raw2 (remove-if-not
				  #'(lambda (x)  (= 0 (rem (colnum x) p)))
				  raw1))
			   (raw3 (remove-if-not
				  #'(lambda (x) (equal (rest x) (take 2 cls)))
				  raw2)))
		      (mapcan
		       #'(lambda (x) (looper (1+ i)
				   (cons (first x) cls)
				   (set-difference rdigs (cons (first x) cls))))
		       raw3)))))
      (time
       (reduce '+
	       (mapcar 'colnum
		       (mapcan #'(lambda (x) (looper 0 x (set-difference digs x)))
			       (remove-if-not
				#'(lambda (x)  (= 0 (rem (colnum x) 17)))
				(permute 3 digs)))))))))


(defparameter refpens (make-array 50 :initial-element nil))

(defun pentagonals (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((num (aref refpens n)))
    (if num
	num
	(setf (aref refpens n) (/ (* n (1- (* 3 n))) 2)))))

(defun list-penta (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (mapcar 'pentagonals (range n 1 1)))

(defun pental? (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((num (/ (1+ (sqrt (1+ (* 24.0l0 n)))) 6)))
    (= (floor num) (ceiling num))))

(defun sumsub-pental? (xs)
  (declare (optimize (speed 3)))
  (let ((num (remove-if-not
	      #'(lambda (x)
		  (and (pental? (+ x (first xs)))
		       (pental? (- (first xs) x))))
	      (rest xs))))
    (if (null num)
	nil
	(list (first xs) (first num)))))

(defun find-pental ()
  (declare (optimize (speed 3)))
  (labels ((inner (i)
	     (declare (fixnum i))
	     (let* ((curp (pentagonals i))
		    (num (remove-if-not
			  #'(lambda (x)
			      (and (pental? (+ curp (pentagonals x)))
				   (pental? (- curp (pentagonals x)))))
			  (range (- i 1) 1 1))))
	       (if (null num)
		   nil
		   (list curp (pentagonals (first num))))))
	   (looper (i)
	      (declare (fixnum i))
	      (let ((res (inner i)))
		(if res res (looper (1+ i))))))
    (time (apply '- (looper 2)))))

(defun dnumcol (n)
  (declare (optimize (speed 3)))
  (labels ((looper (i res)
	      (if (= 10 (length res))
		  res
		  (if (< i 10)
		      (cons i res)
		      (looper (truncate  (/ i 10))
			 (cons (rem i 10) res))))))
    (looper n nil)))


(defun self-power (lim)
  (time (rem (reduce '+ (mapcar #'(lambda (x) (expt x x)) (range 1 lim 1))) (expt 10 10))))

(defun sieves (lim)
  (declare (optimize (speed 3)) (fixnum lim))
  (let ((llim (isqrt lim))
	(refs (make-array lim :initial-element t)))
    (labels ((outer (i res)
	       (declare (optimize (speed 3))
			(fixnum i)
			(dynamic-extent res))
	       (labels ((inner (p)
			  (declare (optimize (speed 3))
				   (fixnum p))
			  (if (< p lim)
			      (progn (setf (aref refs p) nil)
				     (inner (+ p (* 2 i))))
			      (+ 2 i))))
		 (if (< i lim)
		     (if (and (<= i llim) (aref refs i))
			 (progn (inner (* i i))
				(outer (+ i 2)
				       (cons i res)))
			 (outer (+ i 2)
				(if (aref refs i)
				    (cons i res)
				    res)))
		     (reverse res)))))
      (outer 3 (list 2)))))

(defparameter refsprime (make-array 200 :initial-element nil))

(defun prime? (p)
  (declare (optimize (speed 3)) (fixnum p))
  (if (< p 2)
      nil
      (if (= 2 p)
	  t
	  (if (evenp p)
	      nil
	      (let ((refs (aref refsprime p)))
		(if refs
		    (if (= refs 1) nil t)
		    (let ((result (let ((lim (isqrt p)))
				    (labels ((helper (i)
					       (declare (optimize (speed 3))
							(fixnum i))
					       (if (> i lim)
						   t
						   (if (= 0 (rem p i))
						       nil
						       (helper (+ i 2))))))
				      (helper 3)))))
		      (if result
			  (progn (setf (aref refsprime p) 2) t)
			  (progn (setf (aref refsprime p) 1) nil)))))))))

(defun count-power ()
  (+ 3
     (loop for i from 4 to 9
	for sum = (loop for j from 1 to 24
		     for hasil = (ceiling (* j (log i 10)))
		     while (>= (1+ hasil) j)
		     counting (= j hasil) into counter
		     finally (return counter))
	summing sum into summer
	finally (return summer))))



(defun cpower ()
  (declare (optimize (speed 3)))
  (labels ((looper (n m res)
	      (declare (fixnum n m res))
	      (let ((hasil (ceiling (* m (log n 10)))))
		(if (< hasil m)
		    res
		    (looper n (1+ m) (if (= hasil m) (1+ res) res))))))
    (+ 3 (reduce '+ (mapcar #'(lambda (x) (looper x 1 0)) (range 4 9 1))))))

(defun sumdig (n)
  (declare (optimize (speed 3)))
  (labels ((looper (i res)
	      (declare (fixnum res))
	      (if (< i 10)
		  (+ i res)
		  (looper (div i 10) (+ res (rem i 10))))))
    (looper n 0)))



(defun digital-power-sum (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (loop for a from 1 to lim
     for res = (loop for b from 1 to lim
		  for sum = (sumdig (expt a b))
		  maximizing sum into summer
		  finally (return summer))
     maximizing res into mres
     finally (return res)))

(defun take-while (fn ls)
  "Returns the elements of ls starting from first while (fn elmt) is true"
  (if (null ls)
      ls
      (if (not (funcall fn (first ls)))
	  nil
	  (cons (first ls) (take-while fn (rest ls))))))


(defun drop-while (fn ls)
  "Returns the elements of ls starting from first while (fn elmt) is true"
  (if (null ls)
      ls
      (if (funcall fn (first ls))
	  (drop-while fn (rest ls))
	  ls)))

;; Some performance test

(defun cube? (n)
  (declare (optimize (speed 3)))
  (let ((num (expt n 1/3)))
    (= (ceiling num) (floor num))))

(defun ndig-cubes2 (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((find-first (i)
	     (if (cube? i) i (find-first (1+ i))))
	   (find-end (i)
	     (if (cube? i) i (find-end (1- i))))
	   (looper (i j)
	      (declare (fixnum i j))
	      (mapcar #'(lambda (x) (* x x x)) (range i j  1))))
    (let* ((st (find-first (expt 10 (1- n))))
	   (start (truncate (expt st 1/3)))
	   (ed (find-end (- (expt 10 n) 1)))
	   (end (truncate (expt ed 1/3))))
      (looper start end))))

(defun ndig-cubes3 (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((end (expt 10 n)))
    (labels ((looper (i res)
		(let ((num (* i i i)))
		  (if (> num end)
		      res
		      (looper (1+ i) (cons num res))))))
      (looper (ceiling (expt (expt 10 (- n 1)) 1/3)) nil))))

(defun digpermute?2 (a b)
  (declare (optimize (speed 3)))
  (let ((numa (numcol a))
	(numb (numcol b)))
    (equal (sort numa '<) (sort numb '<))))

(defun cubic-permutations2 (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((bahan (ndig-cubes n)))
    (labels ((looper (x xs)
		(let ((res (remove-if-not
			    #'(lambda (m) (digpermute?2 x m))
			    xs)))
		  (if (null res) nil (cons x res)))))
      (remove-if 'null
		 (mapcar #'(lambda (x) (looper x (remove x bahan))) bahan)))))

(defun find-cubes2 (target)
  (declare (optimize (speed 3))
	   (fixnum target))
  (labels ((looper (i)
	      (declare (fixnum i))
	      (let ((res (remove-if-not
			  #'(lambda (x)  (= target (length x)))
			  (cubic-permutations i))))
		(if (null res) (looper (1+ i)) res))))
    (looper 1)))

(defun freq-by (fn lsm)
  (declare (optimize (speed 3)))
  (let ((sls (sort lsm '< :key fn)))
    (labels ((helper (ls p i res1 res)
	       (if (null ls)
		   (cons (list p i) (cons res1 res))
		   (if (= (funcall fn p) (funcall fn (first ls)))
		       (helper (rest ls) p (1+ i) res1 res)
		       (helper (rest ls)
			       (first ls) 1
			       (list p i)
			       (cons res1 res))))))
      (butlast (helper (rest sls) (first sls) 1 nil nil)))))


(defun ndig-cubes (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((end (expt 10 n)))
    (labels ((looper (i res)
		(let ((num (* i i i)))
		  (if (> num end)
		      res
		      (looper (1+ i)
			 (cons (list num (sort (numcol num) '<))
			       res))))))
      (looper (ceiling (expt (expt 10 (- n 1)) 1/3)) nil))))

(defun group-by (fn ls)
  (declare (optimize (speed 3)))
  (labels ((looper (lls res)
	      (if (null lls)
		  res
		  (let* ((tmp (funcall fn (first lls)))
			 (pasukan (remove-if-not
				   #'(lambda (x) (equal (funcall fn x) tmp))
				   lls)))
		    (looper (set-difference lls pasukan)
		       (cons (list tmp pasukan) res))))))
    (looper ls nil)))



(defun cubic-permutations (target)
  (declare (optimize (speed 3))
	   (fixnum target))
  (labels ((looper (i)
	      (declare (fixnum i))
	      (let* ((bahan (group-by 'second (ndig-cubes i)))
		     (tmp (remove-if-not
			   #'(lambda (x) (= (length (second x)) target))
			   bahan)))
		(if (null tmp)
		    (looper (1+ i))
		    (let ((restmp (mapcar #'(lambda (n) (cadr n)) tmp)))
		      (first
		       (mapcar
			#'(lambda (n) (first (sort (mapcar 'car n) '<)))
			restmp)))))))
    (looper 1)))

(defun next-prime (p)
  (declare (optimize (speed 3)) (fixnum p))
  (cond ((= p 2) 3)
	((evenp p) (if (prime? (+ p 1)) (+ p 1) (next-prime (+ p 1))))
	((prime? (+ p 2)) (+ p 2))
	(:otherwise (next-prime (+ p 2)))))

(defun pfactors (p)
  (declare (optimize (speed 3)) (fixnum p))
  (labels ((helper (i n lasti res)
	     (declare (optimize (speed 3)) (fixnum i n lasti))
	     (if (prime? n)
		 (if (= n lasti)
		     res
		     (cons n res))
		 (if (zerop (rem n i))
		     (helper 2 (div n i) i (cons i res))
		     (helper (next-prime i) n lasti res)))))
    (helper 2 p 2 nil)))

(defun cpfactors (p)
  (declare (optimize (speed 3)) (fixnum p))
  (labels ((helper (i n res)
	     (declare (optimize (speed 3)) (fixnum i n))
	     (if (prime? n)
		 (cons n res)
		 (if (zerop (rem n i))
		     (helper 2 (div n i) (cons i res))
		     (helper (next-prime i) n res)))))
    (length (remove-duplicates (helper 2 p nil)))))

(defun disprime (start n)
  (declare (optimize (speed 3))
	   (fixnum start n))
  (labels ((inner (start i scale res sum)
	     (declare (fixnum start i scale res sum))
	     (cond ((< (+ sum scale) n) nil)
		   ((= n sum) (- res (1- n)))
		   (:otherwise 
		    (let* ((num (+ start i))
			   (tmp (cpfactors num)))
		      (cond ((= tmp n)
			     (inner start
				    (1+ i)
				    (1- scale)
				    num
				    (1+ sum)))
			    ((and (not (= tmp n))
				  (>= scale n))
			     (inner start (1+ i) (1- scale) 0 0))
			    (:otherwise nil))))))
	   (looper (start end)
	      (declare (fixnum start end))
	      (let ((scale (- end start 1)))
		(if (< scale n)
		    (looper end (next-prime end))
		    (let ((tmp (inner start 1 scale 0 0)))
		      (if tmp tmp (looper end (next-prime end))))))))
    (looper start (next-prime start))))

(defun disprime2 (n size)
  (declare (optimize (speed 3))
	   (fixnum size n))
  (let* ((tmp (sieves size))
	 (bahan (make-array (length tmp) :initial-contents
			    tmp)))
    (labels ((inner (start i scale res sum)
	       (declare (fixnum start i scale res sum))
	       (cond ((< (+ sum scale) n) nil)
		     ((= n sum) (- res (1- n)))
		     (:otherwise 
		      (let* ((num (+ start i))
			     (tmp (cpfactors num)))
			(cond ((= tmp n)
			       (inner start
				      (1+ i)
				      (1- scale)
				      num
				      (1+ sum)))
			      ((and (not (= tmp n))
				    (>= scale n))
			       (inner start (1+ i) (1- scale) 0 0))
			      (:otherwise nil))))))
	     (looper (i)
		(let* ((start (aref bahan i))
		       (end (aref bahan (1+ i)))
		       (scale (- end start 1)))
		  (if (< scale n)
		      (looper (1+ i))
		      (let ((tmp (inner start 1 scale 0 0)))
			(if tmp tmp (looper (1+ i))))))))
      (looper 0))))



(defun ndig-primes (n diff)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let* ((end (expt 10 (- n 1)))
	 (bahan2 (drop-while
		  #'(lambda (x) (<= x end))
		  (sieves (div (expt 10 n) 3))))
	 (lb2 (length bahan2))
	 (bahan (make-array lb2 :initial-contents bahan2)))
    (labels ((looper (i res)
		(declare (fixnum i res))
		(let* ((num1 (aref bahan i))
		       (num2 (+ diff num1))
		       (num3 (+ diff num2)))
		  (if (and (prime? num2) (prime? num3))
		      (let ((snum1 (sort (numcol num1) '<))
			    (snum2 (sort (numcol num2) '<))
			    (snum3 (sort (numcol num3) '<)))
			(if (and (equal snum1 snum2)
				 (equal snum2 snum3))
			    (if (= 0 res)
				(looper (1+ i) (+ 1 res))
				(list num1 num2 num3))
			    (looper (1+ i) res)))
		      (looper (+ 1 i) res)))))
      (looper 0 0))))

(defun permul (n ls)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((looper (l)
	      (mapcar #'(lambda (x) (cons x l)) ls))
	   (outer (i res)
	     (declare (fixnum i))
	     (if (= i (- n 1))
		 (mapcan #'(lambda (x)
			     (mapcar #'(lambda (y) (cons y x)) (rest ls)))
			 res)
		 (outer (1+ i)
			(mapcan
			 #'(lambda (x) (looper x))
			 res)))))
    (if (= n 1)
	(mapcar 'colnum (mapcar 'list (rest ls)))
	(sort (mapcar 'colnum (outer 1 (mapcar 'list ls))) '<))))

(defun smallmul (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((bahan (range 0 2 1)))
    (labels ((looper (i)
		(let* ((num (permul i bahan))
		       (res (drop-while
			     #'(lambda (x) (not (zerop (rem x n))))
			     num)))
		  (if res
		      (first res)
		      (looper (1+ i))))))
      (looper 1))))

(defparameter refspprime (make-array 100 :initial-element nil))


(defun prev-prime (p)
  (declare (optimize (speed 3)) (fixnum p))
  (if (<= p 2)
      nil
      (if (= p 3)
	  2
	  (let ((tmp (aref refspprime p)))
	    (if tmp
		tmp
		(if (prime? (- p 2))
		    (setf (aref refspprime p) (- p 2))
		    (prev-prime (- p 2))))))))

(defun psuma (i c)
  (declare (optimize (speed 3)) (fixnum i c))
  (cond ((= i 1) 0)
	((= c 2) (if (evenp i) 1 0))
	(:otherwise
	 (labels ((looper (x res)
		     (declare (fixnum x res))
		     (if (> (* x c) i)
			 res
			 (looper (1+ x)
			    (+ res (psuma (- i (* x c))
					  (prev-prime c)))))))
	   (looper 0 0)))))


(defun prime-sum (n target)
  (declare (optimize (speed 3)) (fixnum n target))
  (labels ((looper (i)
	      (declare (fixnum i))
	      (let ((psum (psuma i (prev-prime i))))
		(if (> psum target)
		    (list i psum)
		    (looper (1+ i))))))
    (looper n)))




