
(defun sum (ls)
  (if (null ls)
      0
      (+ (first ls) (sum (rest ls)))))

(defun prod (ls)
  (if (null ls)
      1
      (* (first ls) (prod (rest ls)))))



