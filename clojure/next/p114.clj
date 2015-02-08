(ns poly.p114)

(defn valid?
  [xs]
  (if (every? #{1} xs)
    true
    (let [this (butlast (partition-by identity xs))]
      (every? #(if (= 1 (first %))
                 (>= (count %) 3)
                 true)
              this))))

(defn gen
  [^long n]
  (loop [i (int 0) res [[]]]
    (if (== i n)
      (-> #(if (== 1 (last %))
             (every? #{1} [(% (- n 2)) (% (- n 3))])
             true)
          (filter res))
      (recur (inc i)
             (filter valid?
                     (mapcat #(list (conj % 1)
                                    (conj % 0)) res))))))
