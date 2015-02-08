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
     (cond (= i 0) 1 
           (= c 0) 1
           :else (->> (range)
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

(defn sqr [x] (* x x))

(def psquare?
  (memoize
   (fn [p]
     (let [mp (Math/sqrt p)]
       (= (Math/floor mp) (Math/ceil mp))))))

(defn square-divs
  "Return the number of divisors of n"
  [n]
  (if (= n 1)
    1
    (let [plim (inc (Math/sqrt n))]
      (if (even? n)
        (loop [i 2 lim (quot n 2) res (inc (sqr n))]
          (if (or (> i plim) (>= i lim))
            (if (= 4 n)
              3
              res)
            (if (zero? (rem n i))
              (let [tmp (quot n i)]
                (if (= tmp i)
                  (+ (sqr tmp) res)
                  (recur (inc i)
                         tmp
                         (+ (sqr tmp) (sqr (quot n tmp)) res))))
              (recur (inc i)
                     lim
                     res))))
        (loop [i 3 lim (quot n 3) res (inc (sqr n))]
          (if (or (> i plim) (>= i lim))
            res
            (if (zero? (rem n i))
              (let [tmp (quot n i)]
                (if (= tmp i)
                  (+ (sqr tmp) res)
                  (recur (+ 2 i)
                         tmp
                         (+ (sqr tmp) (sqr (quot n tmp)) res))))
              (recur (+ 2 i)
                     lim
                     res))))))))

(defn euler211
  [lim]
  (loop [i (int 1) res 0]
    (if (>= i lim)
      res
      (recur (inc i)
             (if (psquare? (square-divs i))
               (+ res i)
               res)))))

(defn graph-tour1
  [[gg & gs]]
  (let [conn? (fn [n1 n2 avail]
                (cond (= (first n2) (avail n1)) 
                      [true second]
                      (= (second n2) (avail n1))
                      [true first]
                      :else [false #{}]))
        check (fn check [n1 g avail visited main-node]
                (if (empty? g)
                  true
                  (let [[a & as] g]
                    (if (= visited (count gs))
                      (if (= (sort (conj gs gg))
                             (sort main-node))
                        false
                        (or (check a (conj as n1) first 0
                                   (conj main-node n1))
                            (check a (conj as n1) second 0
                                   (conj main-node n1))))
                      (let [[stat which] (conn? n1 a avail)]
                        (if stat
                          (check a as which (inc visited) 
                                 main-node)
                          (check n1 (conj as a) avail
                                 (inc visited)
                                 main-node)))))))]
    (or (check gg gs first 0 [])
        (check gg gs second 0 []))))

(defn graph-tour2
  [graph]
  (let [conn? (fn [n1 n2 avail]
                (cond (= (first n2) (avail n1)) 
                      [true second]
                      (= (second n2) (avail n1))
                      [true first]
                      :else [false #{}]))
        depth (fn depth [nd gr avail visits]
                (if (or (empty? gr) (nil? gr))
                  true
                  (if (>= visits (count gr))
                    false
                    (let [[a & as] gr
                          [stat which] (conn? nd a avail)]
                      (if stat
                        (depth a as which 0)
                        (depth nd (conj (vec as) a) avail
                               (inc visits)))))))
        check (fn check [[g & gs] nodes]
                (if (or (depth g gs first 0)
                        (depth g gs second 0))
                  true
                  (if (= (sort nodes)
                         (sort graph))
                    false
                    (check (conj (vec gs) g) (conj nodes g)))))]
    (check graph [])))

(fn f
  [g]
  (let [c? (fn [m n l]
             (cond (= (first n) (l m)) 
                   [true second]
                   (= (second n) (l m))
                   [true first]
                   :t [false []]))
        d (fn d [n gr l v]
            (if gr
              (if (>= v (count gr))
                false
                (let [[a & as] gr
                      [s w] (c? n a l)]
                  (if s
                    (d a as w 0)
                    (d n (conj (vec as) a) l
                       (+ 1 v)))))
              true))
        k (fn k [[h & hs] n]
            (if (or (d h hs first 0)
                    (d h hs second 0))
              true
              (if (= (sort n)
                     (sort g))
                false
                (check (conj (vec hs) h) (conj n h)))))]
    (k g [])))

(defn parens
  [n]
  (let [pasang (fn pasang [coll]
                 (if (empty? coll)
                   '(())
                   (conj (concat (map pasang coll)
                                 (map #(conj % '()) coll))
                         (map list coll))))
        comb (fn comb [i coll]
               (if (= i n)
                 (set coll)
                 (comb (inc i)
                       (set (pasang coll)))))]
    (comb 0 #{})))

(defn parens2
  [n]
  (if (zero? n)
    #{""}
    (let [comb (fn comb [coll]
                 (vec (concat (vector (apply (comp vector list) coll))
                              (vector (conj coll '()))
                              (vector (vec (cons '() coll))))))]
      (loop [p #{} i 0]
        (if (= i n)
          (->> (map #(->> (remove #{\space} (str %))
                          butlast rest
                          (apply str)) p)
               set)
          (if (= i 0)
            (recur #{['()]} (inc i))
            (recur (set (mapcat comb p)) (inc i))))))))

(defn parens3
  [n]
  (if (= 0 n)
    #{""}
    (let [fns
          [#(vector (apply list %)) #(conj % '()) #(vec (cons '() %))]
          comb
          (fn comb [fs col] 
            (concat (map #(% col) fns)
                    (fs col)))
          pasang
          (fn pasang [col]
            (let [n (count col)]
              (loop [i 0 dep [] bel (rest col) res []]
                (if (= i n)
                  res
                  (recur (inc i)
                         (take (inc i) col)
                         (drop (+ 2 i) col)
                         (concat res
                                 (map #(vector (concat dep % bel))
                                      (comb pasang (nth col i)))))))))]
      (loop [p #{} i 0]
        (if (= i n)
          (do p
              (->> (map #(->> (remove #{\space} (str %))
                              butlast rest
                              (apply str)) p)
                   set))
          (if (= i 0)
            (recur #{['()]} (inc i))
            (recur (set (mapcat #(comb pasang %) p)) (inc i))))))))












