(ql:quickload :ningle)
(load "clojure.lisp")

(defvar *app* (make-instance 'ningle:<app>))

(defun slurp (fname)
  (open fname :direction :input))

(setf (ningle:route *app* "/")
      (slurp "one.html"))

(clack:clackup *app*)
