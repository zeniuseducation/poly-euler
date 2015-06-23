(load "clojure.lisp")

(defun prime? (p)
  (deff p)
  (let ((llim (isqrt p)))
    (cloop (i 3)
	   (deff i)
	   (if (> i llim)
	       true
	       (if (= 0 (rem p i))
		   false
		   (recur (+ i 2)))))))

(defun sol5 (lim)
  (deff lim)
  (let ((refs (->> (cons 1 (range 1 lim))
		   (make-array (+ lim 1) :initial-contents))))
    (cloop (i 2 res 1) (deff i res)
	   (if (> i lim)
	       res
	       (let ((tmpi (aref refs i)))
		 (progn (cloop (j (+ i 1)) (deff j)
			       (if (> j lim)
				   false
				   (let ((tmpj (aref refs j)))
				     (if (= 0 (rem tmpj tmpi))
					 (progn (setf (aref refs j)
						      (div tmpj tmpi))
						(recur (+ j 1)))
					 (recur (+ j 1))))))
			(recur (+ i 1) (* res tmpi))))))))

(defun sol10 (lim)
  (deff lim)
  (let ((refs (make-array (+ lim 1) :initial-element true))
	(llim (isqrt lim)))
    (cloop (i 3 res 2) (deff i res)
	   (if (> i lim)
	       res
	       (if (aref refs i)
		   (if (<= i llim)
		       (progn
			 (cloop (j (* i i)) (deff j)
				(if (> j lim)
				    nil
				    (progn
				      (setf (aref refs j) false)
				      (recur (+ j (* 2 i))))))
			 (recur (+ i 2)
				(+ res i)))
		       (recur (+ i 2)
			      (+ res i)))
		   (recur (+ i 2) res))))))

(defun modex (a b modi)
  (deff a b modi)
  (cond ((= b 0) 1)
	((= b 1) (rem a modi))
	(:else (let ((modexi (modex a (div b 2) modi)))
		 (if (evenp b)
		     (rem (* modexi modexi) modi)
		     (rem (* a modexi modexi) modi))))))

(defun sol48 (lim modi)
  (deff lim modi)
  (rem (->> (range 1 lim)
	 (mapcar (fn (modex % % modi)))
	 (reduce '+)) modi))








