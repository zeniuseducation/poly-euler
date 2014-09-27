(load "clojure.lisp")

;; PROBLEM 85

(defun csrec (i j m n)
  "Returns the number of possible i x j rectangles plotted into an m x n plane"
  (* (1+ (- m i))
     (1+ (- n j))))

(defun crec (m n)
  "Returns the number of arbitrary-sized rectangles that can be
plotted into m by n plane"
  (sum (loop for i from 1 to m
	  append (loop for j from 1 to n
		    collect (csrec i j m n)))))

(defun sol85 (size lim)
  "Returns the rectangle 'size' which rectangles closest to 2 millions"
  (first (sort (loop for i from 1 to size
		  append (loop for j from 1 to size
			    collect (list (* i j)
					  (abs (- lim (crec i j))))))
	       '< :key 'second)))

(defun euler85 (size lim)
  (time (sol85 size lim)))

"Elapsed time for 90x90 0.57 seconds"

