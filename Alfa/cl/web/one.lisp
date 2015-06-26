(load "clojure.lisp")

(defun str (&rest args)
  (apply 'concatenate 'string args))

(defun create-db (host dbname)
  (str host dbname))

(defvar cdb (create-db "http://localhost:5984/" "sibego"))


