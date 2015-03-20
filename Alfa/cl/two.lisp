;; (load "clojure.lisp")

(defun sum-sieve (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (let ((refs (make-array (+ 1 lim)
			  :initial-element true))
	(llim (isqrt lim)))
    (loop for i from 3 to lim by 2
       when (aref refs i)
       summing i into sum
       when (<= i llim)
       do (loop for j from (* i i) to lim by (* i 2)
	     do (setf (aref refs j) false))
       finally (return (+ 2 sum)))))

(defun prime? (n)
  (deff n)
  (cloop (i 3) (deff i)
	 (if (> (* i i) n)
	     true
	     (if (= 0 (rem n i))
		 false
		 (recur (+ i 2))))))

(defun permute (n xs)
  (deff n)
  (cloop (i 1 res (mapcar (fn (list %)) xs))
	 (deff i)
	 (if (= i n)
	     res
	     (->> (loop for x in
		       (remove-if
			(fn (member % r)) xs)
		     collect (cons x r))
		  (loop for r in res append)
		  (recur (1+ i))))))

(defun permutes (n xs)
  (deff n)
  (cloop (i 1 res (mapcar (fn (list %)) xs)) (deff i)
	 (if (= i n)
	     res
	     (->> (loop for x in xs collect (cons x r))
		  (loop for r in res append)
		  (recur (1+ i))))))

(defun abun? (m)
  (deff m)
  (if (evenp m)
      (cloop (i 2 res 1) (deff i res)
	     (cond
	       ((> res m) t)
	       ((> (* i i) m) nil)
	       ((= (* i i) m) (> (+ res i) m))
	       ((zerop (rem m i)) (recur (+ 1 i)
					 (+ res i (div m i))))
	       (t (recur (+ i 1) res))))
      (cloop (i 3 res 1) (deff i res)
	     (cond
	       ((> res m) t)
	       ((> (* i i) m) nil)
	       ((= (* i i) m) (> (+ res i) m))
	       ((zerop (rem m i)) (recur (+ 1 i)
					 (+ res i (div m i))))
	       (t (recur (+ i 2) res))))))

(defun sol23 (lim)
  (deff lim)
  (let ((abuns (make-array (+ lim 1) :initial-element false))
	(sum-abuns (make-array (+ lim 1) :initial-element false)))
    (progn
      (cloop (i 12) (deff i)
	     (if (< i lim)
		 (recur (progn (if (abun? i)
				   (setf (aref abuns i) true))
			       (+ i 1)))
		 i))
      (cloop (i 12 res (div (* lim (+ lim 1)) 2)) (deff i res)
	     (if (< i (div lim 2))
		 (if (aref abuns i)
		     (let ((tmp
			    (cloop (j i resj 0)  (deff j resj)
				   (if (> (+ i j) lim)
				       resj
				       (if (aref abuns j)
					   (if (aref sum-abuns (+ i j))
					       (recur (+ j 1) resj)
					       (progn (setf (aref sum-abuns (+ i j))
							    true)
						      (recur (+ i j)
							     (+ resj (+ i j)))))
					   (recur (+ j 1) resj))))))
		       (recur (+ i 1) (- res tmp)))
		     (recur (+ i 1) res))
		 res)))))

(defun fact (i)
  (if (<= i 1) 1 (* i (fact (1- i)))))

(defvar digfacts
  (->> (mapcar 'fact (range 9))
       (make-array 10 :initial-contents)))

(defun sol34 (lim)
  (cloop (i 100 res 0) (deff i res)
	 (if (> i lim)
	     res
	     (let ((sum-digfactorial
		    (cloop (j i resj 0) (deff j resj)
			   (if (= j 0)
			       resj
			       (recur (div j 10)
				      (+ resj (aref digfacts (rem j 10))))))))
	       (if (= i sum-digfactorial)
		   (recur (+ i 1) (+ res i))
		   (recur (+ i 1) res))))))

(defun sol34b (lim)
  (labels ((sumdfact (n &optional (res 0))
	     (if (zerop n)
		 res
		 (sumdfact (div n 10)
			   (+ res (aref digfacts (rem n 10)))))))
    (loop for i from 100 to lim
       when (= i (sumdfact i))
       summing i into sum
       finally (return sum))))





