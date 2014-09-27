
(load "clojure.lisp")


(defun find-blue (b tot)
  "Returns the number of blue required to meet the prob, if no
possible int value, returns nil"
  (let ((tmp (/ (* b (dec b)) (* tot (dec tot)))))
    (if (> tmp 1/2)
	nil
	(if (= 1/2 tmp)
	    (list tot b)
	    (find-blue (inc b) tot)))))

(defun find-total (start)
  "Returns the next arrangements that returns int value both for total and blue"
  (let* ((b (floor (* start (/ 1 (sqrt 2)))))
	 (blue (find-blue b start)))
    (if (null blue)
	(find-total (inc start))
	blue)))

(defun arrangements (ls target ratio)
  "Returns the arrangement which total exceeds target"
  (let ((next-arr (find-total (round (* ratio (first ls))))))
    (if (> (first next-arr) target)
	next-arr
	(arrangements next-arr
		      target
		      (floor (/ (first next-arr)
				(first ls)))))))

(defun solution (target)
  (time (arrangements '(4 0 0) target 2)))



