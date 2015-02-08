(ns prob169)

(def coins [1,2,5,10,20,50,100,200])

(def expt
  (memoize (fn [a m]
             (reduce *' (repeat m a)))))

(def sumas
  (memoize
   (fn [^long i ^long c]
     (cond (== i 0) 1 
           (== c 0) 1
           :else (->> (iterate inc 0)
                      (take-while #(<= (* % (nth coins c)) i))
                      (map #(sumas (- i (* % (nth coins c)))
                                   (- c 1)))
                      (reduce +))))))

(def suma2
  (memoize
   (fn [i c]
     (cond (== i 0) 1 
           (== c 0) 1
           :else (->> (iterate inc 0)
                      (take-while #(<= (* % (expt 2 c)) i))
                      (map #(suma2 (- i (* % (expt 2 c)))
                                   (- c 1)))
                      (reduce +))))))

(defn suma-2s
  [n]
  (suma2 n (dec (first (drop-while #(< (expt 2 %) n) (range))))))

(defn ^long suma-coins
  [^long n]
  (sumas n 7))
