(load "clojure.lisp")

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



