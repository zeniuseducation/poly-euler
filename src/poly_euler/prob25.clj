(ns poly-euler.prob25)

(defn expt
  [a m]
  (if (zero? m) 1 (*' a (expt a (dec m)))))

(defn euler25
  [lim [a b] i]
  (if (>= a lim)
    i
    (euler25 lim [(+' a b) a] (inc i))))
