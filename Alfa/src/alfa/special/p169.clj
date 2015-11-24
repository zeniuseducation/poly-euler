(ns alfa.special.p169)

(def expt
  (memoize
    (fn [^long a ^long m]
      (cond
        (== m 0) 1
        (== m 1) a
        :else (let [res (expt a (quot m 2))]
                (if (even? m) (*' res res) (*' a res res)))))))

(def lim (expt 10 25))

(def exp2 (partial expt 2))

(def exps
  (->> (sequence
         (comp (map exp2)
               (take-while (partial > lim)))
         (range))
       (reverse)))

(def iter
  (memoize
    (fn [cur ep kol]
      (cond
        (== cur 0) 1
        (< cur 0) 0
        (< ep 0) 0
        kol (+ (iter (- cur (exp2 ep)) ep false)
               (reduce + (map #(iter (- cur (exp2 ep)) (- ep %) true) (range 1 ep)))
               (reduce + (map #(iter cur (- ep %) true) (range 1 ep))))
        :else (+ (reduce + (map #(iter (- cur (exp2 ep)) (- ep %) true) (range 1 ep)))
                 (reduce + (map #(iter cur (- ep %) true) (range 1 ep))))))))

