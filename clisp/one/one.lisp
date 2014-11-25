(defun div (a b)
  (declare (optimize (speed 3)) (fixnum a b))
  (truncate (/ a b)))

(defun find-cycle (n)
  "Find the number of recurring digits in 1/n"
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((refs (make-array n :initial-element nil))
	(refs2 (make-array n :initial-element nil)))
    (labels ((outer (i res res2)
	       (declare (optimize (speed 3))
			(fixnum i res res2))
	       (if (aref refs2 i)
		   res2
		   (let ((rems (rem (* 10 i) n)))
		     (if (= 0 rems)
			 0
			 (if (aref refs i)
			     (progn (setf (aref refs2 i) t)
				    (outer rems res (+ 1 res2)))
			     (progn (setf (aref refs i) t)
				    (outer rems (+ 1 res) res2))))))))
      (outer 1 0 0))))

(defun max-cycle (lim)
  "Returns the n in the range 2-lim of which recurring digits achieve maximum"
  (declare (optimize (speed 3))
	   (fixnum lim))
  (labels ((helper (i n res)
	     (declare (optimize (speed 3))
		      (fixnum i n res))
	     (if (> res i)
		 (list n res)
		 (let ((tmp (find-cycle i)))
		   (if (> tmp res)
		       (helper (- i 1) i tmp)
		       (helper (- i 1) n res))))))
    (helper lim lim 0)))

(defun euler28 (lim)
  (declare (optimize (speed 3)) (fixnum lim))
  (labels ((looper (a b res i)
	      (declare (fixnum a b res i))
	      (if (= i lim)
		  res
		  (let ((tempa (+ a (+ 8 (- a b)))))
		    (looper tempa a (+ res (* 4 tempa)) (+ 2 i))))))
    (looper 6 1 25 3)))

(defun prime? (p)
  (declare (optimize (speed 3)) (fixnum p))
  (if (< p 2)
      nil
      (if (= 2 p)
	  t
	  (if (evenp p)
	      nil
	      (let ((lim (isqrt p)))
		(labels ((helper (i)
			   (declare (optimize (speed 3)) (fixnum i))
			   (if (> i lim)
			       t
			       (if (= 0 (rem p i))
				   nil
				   (helper (+ i 2))))))
		  (helper 3)))))))

(defun next-prime (p)
  (declare (optimize (speed 3)) (fixnum p))
  (cond ((= p 2) 3)
	((prime? (+ p 2)) (+ p 2))
	(:otherwise (next-prime (+ p 2)))))

(defun prev-prime (p)
  (declare (optimize (speed 3)) (fixnum p))
  (cond ((<= p 2) nil)
	((= p 3) 2)
	((prime? (- p 2)) (- p 2))
	(:otherwise (prev-prime (- p 2)))))

(defun euler27 (lim)
  (declare (optimize (speed 3)) (fixnum lim))
  (labels ((blooper (b resb)
	     (declare (fixnum b))
	     (labels ((alooper (a cura resa)
			(declare (fixnum a cura resa))
			(labels ((nlooper (n resn)
				   (declare (fixnum n resn))
				   (if (prime? (+ (* n n) (* a n) b))
				       (nlooper (+ 1 n) (+ 1 resn))
				       resn)))
			  (cond
			    ((> a lim)
			     (list resa cura b))
			    ((<= (+ a b 1) 0)
			     (alooper (+ 1 a) cura resa))
			    (:otherwise
			     (let ((resn (nlooper 1 1)))
			       (if (> resn resa)
				   (alooper (+ 2 a) a resn)
				   (alooper (+ 2 a) cura resa))))))))
	       (if (< b (first resb))
		   (* (second resb) (third resb))
		   (let ((tmpres (alooper (- lim) (- lim) 1)))
		     (if (> (first tmpres) (first resb))
			 (blooper (prev-prime b) tmpres)
			 (blooper (prev-prime b) resb)))))))
    (blooper 997 (list 0 0 0))))

(defun euler27b (lim)
  (declare (optimize (speed 3)) (fixnum lim))
  (labels ((blooper (b resb)
	     (declare (fixnum b))
	     (labels ((alooper (a cura resa)
			(declare (fixnum a cura resa))
			(labels ((nlooper (n resn)
				   (declare (fixnum n resn))
				   (if (prime? (+ (* n n) (* a n) b))
				       (nlooper (+ 1 n) (+ 1 resn))
				       resn)))
			  (cond
			    ((< a (- lim))
			     (list resa cura b))
			    ((<= (+ a b 1) 0)
			     (alooper (- a 2) cura resa))
			    (:otherwise
			     (let ((resn (nlooper 1 1)))
			       (if (> resn resa)
				   (alooper (- a 2) a resn)
				   (alooper (- a 2) cura resa))))))))
	       (if (< b (first resb))
		   (* (second resb) (third resb))
		   (let ((tmpres (alooper lim lim 1)))
		     (if (> (first tmpres) (first resb))
			 (blooper (prev-prime b) tmpres)
			 (blooper (prev-prime b) resb)))))))
    (blooper 997 (list 0 0 0))))

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

(defun euler27a (lim)
  (declare (optimize (speed 3)) (fixnum lim))
  (reduce '*
	  (rest
	   (first
	    (sort (loop for b in (sieves lim)
		     collect
		       (labels
			   ((outer (a cura res)
			      (declare (fixnum cura res))
			      (if (null a)
				  (list res cura b)
				  (labels ((inner (n resa)
					     (declare (fixnum n resa))
					     (if (prime? (+ (* n n)
							    (* (first a) n)
							    b))
						 (inner (+ 1 n) (+ 1 resa))
						 resa)))
				    (let ((resn (inner 1 1)))
				      (if (> resn res)
					  (outer (rest a) (first a) resn)
					  (outer (rest a) cura res)))))))
			 (outer (remove-if-not
				 #'(lambda (x) (> (+ 1 x b) 0))
				 (loop for m from (- lim) to lim collect m))
				(- lim)
				1)))
		  '> :key 'first)))))

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

(defun sum-pow5a (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((looper (i res)
	      (declare (fixnum i res))
	      (if (< i 10)
		  (+ res (expt i 5))
		  (looper (truncate (/ i 10))
		     (+ res (expt (rem i 10) 5))))))
    (looper n 0)))

(defun sum-fif (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (reduce '+ (mapcar #'(lambda (x) (expt x 5)) (numcol n))))

(defun range (i j k)
  (declare (optimize (speed 3))
	   (fixnum i j k))
  (if (< i j)
      (loop for m from i to j by k collect m)
      (loop for m from i downto j by k collect m)))

(defparameter refs5 (make-array 10
				:initial-contents
				(mapcar #'(lambda (x) (expt x 5))
					(range 0 9 1))))

(defun sum-pow5 (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((looper (i res)
	      (declare (fixnum i res))
	      (if (< i 10)
		  (+ res (aref refs5 i))
		  (looper (div i 10)
		     (+ res (aref refs5 (rem i 10)))))))
    (looper n 0)))

(defun all-sum5 (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (labels ((looper (m res)
	      (declare (fixnum m res))
	      (if (> m lim)
		  res
		  (let ((summ (sum-pow5 m)))
		    (if (= summ m)
			(looper (+ 1 m) (+ res summ))
			(looper (+ 1 m) res))))))
    (looper 10 0)))

(defun sumfif? (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((looper (i res)
	      (declare (fixnum i res))
	      (if (< i 10)
		  (= n (+ res (aref refs5 i)))
		  (if (> res n)
		      nil
		      (looper (div i 10)
			 (+ res (aref refs5 (rem i 10))))))))
    (looper n 0)))

(defun euler30 (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (labels ((looper (m res)
	      (declare (fixnum m res))
	      (if (= 10 m)
		  res
		  (if (sumfif? m)
		      (looper (- m 1) (+ m res))
		      (looper (- m 1) res)))))
    (looper lim 0)))

(defun euler30a (lim)
  (declare (optimize (speed 3)))
  (labels ((looper (m res)
	      (declare (fixnum m res))
	      (if (= 10 m)
		  res
		  (let ((summ (sum-pow5 m)))
		    (if (= summ m)
			(looper (- m 1) (+ res summ))
			(looper (- m 1) res))))))
    (looper lim 0)))


(defun take-while (f ls)
  (declare (optimize (speed 3)))
  (if (funcall f (first ls))
      (cons (first ls) (take-while f (rest ls)))
      '()))

(defun iterate (f g i)
  (declare (optimize (speed 3)))
  (if (not (funcall g i))
      '()
      (append (iterate f g (funcall f i))
	      (list i))))

(defparameter cs (make-array 8 :initial-contents '(1 2 5 10 20 50 100 200)))

(defun suma-coins (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((sumas (i c)
	     (declare (fixnum i c))
	     (labels ((inner (x res)
			(declare (fixnum x res))
			(if (< i (* x (aref cs c)))
			    res
			    (inner (+ 1 x)
				   (+ res (sumas (- i (* x (aref cs c)))
						 (- c 1)))))))
	       (cond ((= i 0) 1)
		     ((= c 0) 1)
		     (:else (inner 0 0))))))
    (sumas n 7)))





(defun suma-ints (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((isuma (i c)
	     (declare (optimize (speed 3)) (fixnum i c))
	     (labels ((inner (x res)
			(declare (optimize (speed 3))
				 (fixnum x res))
			(if (< i (* x c))
			    res
			    (inner (+ 1 x)
				   (+ res (isuma (- i (* x c))
						 (- c 1)))))))
	       (cond ((= i 0) 1)
		     ((= c 1) 1)
		     (:else (inner 0 0))))))
    (isuma n (- n 1))))

(defun suma-ints2 (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((refs (make-array (+ 1 n)
			  :initial-element
			  (make-array (+ 1 n)
				      :initial-element nil))))
    (labels ((isuma (i c)
	       (declare (optimize (speed 3)) (fixnum i c))
	       (labels ((inner (x res)
			  (declare (optimize (speed 3))
				   (fixnum x res))
			  (if (< i (* x c))
			      res
			      (inner (+ 1 x)
				     (+ 1 (isuma (- i (* x c))
						 (- c 1)))))))
		 (cond ((= i 0) 1)
		       ((= c 1) 1)
		       (:else (let ((fromref (aref (aref refs i) c)))
				(if fromref
				    fromref 
				    (setf (aref (aref refs i) c)
					  (inner 0 0)))))))))
      (isuma n (- n 1)))))

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

(defun pandig-prime (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((bahan (range n 1 1)))
    (labels ((looper (n)
		(declare (fixnum n))
		(if (= n 0)
		    nil
		    (let* ((res (mapcar #'(lambda (x) (colnum (append (take n bahan) x)))
					(permutations (drop n bahan))))
			   (res1 (remove-if-not 'prime? res)))
		      (if res1
			  (apply 'max res1)
			  (looper (- n 1)))))))
      (looper (- n 1)))))

(defun permute (n ls)
  (declare (optimize (speed 3))
	   (fixnum n))
  (if (= 1 n)
      (mapcar 'list ls)
      (mapcan #'(lambda (s)
		  (mapcar #'(lambda (x)
			      (cons s x))
			  (permute (- n 1) (remove s ls)))) ls)))

(defun pandig-mul (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let ((ls (range 1 n 1)))
    (labels ((inner (js i)
	       (declare (optimize (speed 3)) (fixnum i))
	       (if (>= i (- n (length js)))
		   nil
		   (let* ((res (set-difference ls js))
			  (tmp (permute i res)))
		     (append (mapcar
			      #'(lambda (x) (list (colnum js)
					     (colnum x)
					     (set-difference res x)))
			      tmp)
			     (inner js (1+ i))))))
	     (outer (j)
	       (declare (optimize (speed 3)) (fixnum j))
	       (if (> j (/ n 2))
		   nil
		   (append (mapcan #'(lambda (x) (inner x 1))
				   (permute j ls))
			   (outer (+ 1 j))))))
      (remove-duplicates
       (mapcar #'(lambda (x) (* (first x) (second x)))
	       (remove-if-not
		#'(lambda (k)
		    (equal (sort (numcol (* (first k) (second k)))
				 '<)
			   (third k)))
		(outer 1)))))))

(defun pandig? (ls)
  (declare (optimize (speed 3)))
  (equal (sort ls '<) (range 1 9 1)))

(defun pandig-products (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (remove-duplicates
   (loop for i from 1 to lim
      append (loop for j from (1+ i) to lim
		when (let ((pandigs (append (numcol i)
					    (numcol j)
					    (numcol (* i j)))))
		       (pandig? pandigs))
		collect (* i j)))))


(defun pandig-mulb (lim)
  (declare (optimize (speed 3)))
  (labels ((inner (i j)
	     (declare (fixnum i j))
	     (if (> (* i j) (* 3 lim))
		 nil
		 (let ((res (append (numcol i)
				    (numcol j)
				    (numcol (* i j)))))
		   (if (pandig? res)
		       (cons (* i j) (inner i (+ 1 j)))
		       (inner i (+ 1 j))))))
	   (outer (i)
	     (declare (fixnum i))
	     (if (> (* i i) lim)
		 nil
		 (append (inner i (+ 1 i))
			 (outer (+ 1 i))))))
    (reduce '+ (remove-duplicates (outer 2)))))

(defun circular-prime?3 (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (let* ((xs (numcol n))
	 (lxs (length xs))
	 (cycle (mapcar
		 #'(lambda (x) (colnum (append (drop x xs)
					  (take x xs))))
		 (range 0 (- lxs 1) 1))))
    (= 0 (length (remove-if 'prime? cycle)))))

(defun circular-prime? (n)
  (declare (optimize (speed 3))
	   (fixnum n))
  (if (member n '(1 9))
      nil
      (let ((res (floor (log n 10))))
	(labels ((looper (m i)
		    (if (> i (* 2 res))
			t
			(if (prime? m)
			    (let ((tmp (truncate (/ m 10))))
			      (looper (+ tmp (* (rem m 10) (expt 10 res)))
				 (+ 1 i)))
			    nil))))
	  (looper n 1)))))

(defun all-cprimes (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (let ((bahan (list 1 3 7 9)))
    (labels ((looper (i)
		(declare (optimize (speed 3))
			 (fixnum i))
		(if (> i lim)
		    0
		    (if (circular-prime? i)
			(1+ (reduce '+ 
				    (mapcar
				     #'(lambda (x) (looper (+ (* 10 i) x)))
				     bahan)))
			(reduce '+
				(mapcar
				 #'(lambda (x) (looper (+ (* 10 i) x)))
				 bahan))))))
      (+ 2 (reduce '+ (mapcar #'looper bahan))))))

(defun all-cprimes2 (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (length (remove-if-not 'circular-prime? (sieves lim))))

(defun bin-palin? (n)
  "Returns true if n is palindrome in base 2"
  (declare (optimize (speed 3))
	   (fixnum n))
  (labels ((bincol (i res)
	     (declare (fixnum i))
	     (if (< i 2)
		 (append res (list i))
		 (bincol (truncate (/ i 2))
			 (append res (list (rem i 2)))))))
    (let ((tmp (bincol n nil)))
      (equal tmp (reverse tmp)))))

(defun bi-palins (n)
  "Returns the sum of all 1-n-digit (in base 10) double-base palindromes"
  (declare (optimize (speed 3))
	   (fixnum n))
  (if (= n 1)
      (reduce '+ (remove-if-not 'bin-palin? (range 1 10 2)))
      (let* ((expn (- (truncate (/ n 2)) 1))
	     (start (expt 10 expn))
	     (end (expt 10 (1+ expn))))
	(labels ((evenpals (i res)
		   "The one that generate even digit palindromes"
		   (declare (fixnum i res))
		   (if (= i end)
		       res
		       (let* ((tmp (numcol i))
			      (num (colnum (append tmp (reverse tmp)))))
			 (if (= 0 (rem num 2))
			     (evenpals (+ 1 i) res)
			     (if (bin-palin? num)
				 (evenpals (+ 1 i) (+ res num))
				 (evenpals (+ 1 i) res))))))
		 (oddpals (i res)
		   "The one that generate odd digit palindromes"
		   (declare (fixnum i res))
		   (if (= i end)
		       res
		       (let* ((tmp (numcol i))
			      (nums (mapcar
				     #'(lambda (x) (colnum (append tmp
							      (list x)
							      (reverse tmp))))
				     (list 0 1 2 3 4 5 6 7 8 9))))
			 (oddpals (+ 1 i)
				  (+ res
				     (reduce '+
					     (remove-if-not 'bin-palin?
						 (remove-if 'evenp nums)))))))))
	  (if (evenp n)
	      (evenpals start 0)
	      (oddpals start 0))))))

(defun sum-bipalins (n)
  "Returns the sums of all double bases palindromes upto 10^n"
  (declare (optimize (speed 3))
	   (fixnum n))
  (reduce '+ (mapcar 'bi-palins (range 1 n 1))))




















