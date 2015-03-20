(ns alfa.extra)

(defn factorial [n]
  (if (<= n 1) 1
    (* n (factorial (dec n)))))

(defn solution
  [lim]
  (let [the-factorials (mapv factorial (range 0 10))
        digit-factorial (fn [n]
                          (transduce
                           (map the-factorials)
                           + (map #(- (int %) (int \0)) n)))]
    (transduce
     (comp (map #(vector % (digit-factorial (str %)))) 
           (keep #(let [[i j] %] (if (== i j) i nil))))
     + (range 100 lim))))

(defn ^long sol34
  [^long lim]
  (let [the-factorials (mapv factorial (range 0 10))]
    (loop [i (int 100) res (int 0)]
      (if (> i lim)
        res
        (let [difact (loop [n (int i) res (int 0)]
                       (if (< n 10)
                         (+ res (the-factorials n))
                         (recur (quot n 10)
                                (+ res (the-factorials (rem n 10))))))]
          (if (== i difact)
            (recur (+ i 1) (+ res i))
            (recur (+ i 1) res)))))))

(def theFactorials
  (zipmap (range 0 10) (map factorial (range 0 10))))

(defn isValid [n]
  (== n (reduce + (map theFactorials
                       (map #(- (int %) (int \0)) (str n))))))

(defn solution1
  [lim]
  (transduce
   (comp (map #(vector % (map (fn [n] (- (int n) (int \0))) (str %))))
         (map #(let [[i j] %] [i (reduce + (map the-factorials j))]))
         (keep #(let [[i j] %] (if (== i j) i nil))))
   + (range 100 lim)))


