(defn expt
  [a m]
  (if (zero? m)
    1
    (* a (expt a (dec m)))))

(defn prime?
  [^long n]
  (cond (< n 2) false
        (== n 2) true
        (even? n) false
        :otherwise (let [lim (inc (Math/sqrt n))]
                     (loop [i (int 3)]
                       (cond (> i lim) true
                             (== 0 (rem n i)) false
                             :otherwise (recur (inc i)))))))



