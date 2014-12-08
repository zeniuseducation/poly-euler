(load "clojure.lisp")

(defun sqrs (lim)
  (deff lim)
  (->> (range lim)
       (cmap (fn (* % %)))
       (reduce '+)))

(defun cubes (lim)
  (deff lim)
  (-> (fn (* % % %))
      (cmap (range lim))))



