(ns euler.prob259)

(load-file "math.clj")

(def mats (range 1 10))

(defn scat
  [a b]
  (read-string (str a b)))

(def oprs [+ - / * scat])

(defn deepop
  [dig ops]
  (loop [lo ops res []]
    (if (empty? lo)
      res
      (recur (rest lo)
             (conj res (apply (first lo) dig))))))

