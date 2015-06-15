(ns alfa.two)

(defn square
  [x]
  (* x x))

(defn leven
  ([s1 s2] (leven s1 s2 0))
  ([s1 s2 n]
     (loop [[x1 & xs1 :as l1] s1
            [x2 & xs2 :as l2] s2 res n]
       (if (= l1 l2)
         res
         (if x1
           (if x2
             (if (= x1 x2)
               (recur xs1 xs2 res)
               (let [c1 (count l1) c2 (count l2)]
                 (cond (= c1 c2) (recur xs1 xs2 (inc res))
                       (> c1 c2) (apply min [(leven xs1 xs2 (inc res))
                                             (leven xs1 l2 (inc res))])
                       :else (apply min [(leven xs1 xs2 (inc res))
                                         (leven l1 xs2 (inc res))]))))
             (+ res (count l1)))
           (+ res (count l2)))))))

(defn str-trim
  [s]
  (clojure.string/replace s #" " ""))

(defn sol111
  [s cw]
  (let [f1 (fn [s1] (clojure.string/replace s1 #" " ""))
        f3 (fn [s3] (clojure.string/split s3 #"#"))
        f4 (fn [vs] (let [sc1 (mapv f1 vs)
                         c1 (count (first sc1))]
                     (apply concat
                            (for [i (range c1)]
                              (f3 (apply str (mapv #(nth % i) sc1)))))))
        f5 (fn [vs] (apply concat (map #(f3 (f1 %)) vs)))
        fit? (fn [s1 s2]
               (if (= (count s1) (count s2))
                 (loop [[ss1 & sr1] s1 [ss2 & sr2] s2]
                   (if ss1
                     (if (or (= ss1 ss2) (= ss2 \_))
                       (recur sr1 sr2)
                       false)
                     true))
                 false))]
    (or (if (some #(fit? s %) (f5 cw)) true false)
        (if (some #(fit? s %) (f4 cw)) true false))))

(defn sol138
  [n lim]
  (let [b (->> (iterate #(* % %) n)
               (take-while #(<= % lim))
               (mapcat str)
               (apply str))
        cb (count b)
        size (->> (iterate inc 1)
                  (map #(* % %))
                  (drop-while #(< % cb))
                  first)
        t (apply str (concat b (repeat (- size cb) "*")))]
    (condp = size
      1 [t]
      4 [(str " " (nth t 0) " ")
         (str (nth t 3) " " (nth t 1))
         (str " " (nth t 2) " ")]
      9 [(str "  " (nth t 6) "  ")
         (str " " (nth t 5) " " (nth t 7) " ")
         (str (nth t 4) " " (nth t 0) " " (nth t 8))
         (str " " (nth t 3) " " (nth t 1) " ")
         (str "  " (nth t 2) "  ")]
      16 [(str "   " (nth t 6) "   ")
          (str "  " (nth t 5) " " (nth t 7) "  ")
          (str " " (nth t 4) " " (nth t 0) " " (nth t 8) " ")
          (apply str (interpose " " (map #(nth t %) [15 3 1 9])))
          (str " " (apply str (interpose " " (map #(nth t %) [14 2 10]))) " ")
          (str "  " (nth t 13) " " (nth t 11) "  ")
          (str "   " (nth t 12) "   ")])))

(defn f195a
  [n]
  (cond
   (= n 0) #{""}
   (= n 1) #{"()"}
   :else (loop [i 1 res #{[]}]
           (if (= n i)
             (set res)
             (recur (inc i)
                    (->> (for [r res]
                           [(vector r)
                            (vec (cons [] r))
                            (conj r [])])
                         (apply concat)
                         (concat (map vector res))
                         set))))))

(defn f195b
  [n]
  (cond
   (= n 0) #{""}
   (= n 1) #{"()"}
   :else
   (loop [i 1 res #{" ( ) "}]
     (if (= n i)
       (set (map #(clojure.string/replace % #" " "") res))
       (recur (inc i)
              (->> (for [r res]
                     (concat [(str " ( " r " ) ")
                              (str " ( ) " r)
                              (str r " ( ) ")]
                             (let [rs (clojure.string/split r #" ")]
                               (map #(str " "
                                          (apply str (take % rs))
                                          " ( ) "
                                          (apply str (drop % rs)))
                                    (range (inc (count rs)))))))
                   (apply concat) 
                   set))))))



(defn f195 [n]
  (if (= n 0) #{""}
      (loop [i 1 res #{"()"}]
        (if (= n i)
          res
          (recur (inc i)
                 (->> (for [r res]
                        (cons (str "(" r ")")
                              (map #(str (apply str (take % r))
                                         "()"
                                         (apply str (drop % r)))
                                   (range (count r)))))
                      (apply concat)
                      set))))))









