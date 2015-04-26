(ns alfa.p200
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]))

(def power-twos
  (map #(expt 2 %) (range)))

(def map-power
  (let [powers (->> power-twos
                    (take-while #(< % (expt 10 25)))
                    reverse)]
    (->> (mapcat #(list [(str "p" %1 "a") %2]
                        [(str "p" %1 "b") %2])
                 (range) powers)
         (into {}))))

(def keymap
  (set (keys map-power)))

(defn binrep
  [n]
  (let [bahan (reverse (take-while #(<= % n) power-twos))
        raws (interleave bahan bahan)
        ctr (count raws)]
    ctr))

(defn ^long sol193
  [^long lim]
  (let [refs (boolean-array (+ lim 1) true)
        llim (long (Math/sqrt lim))]
    (loop [res (* 0.75 (* lim lim)) i (int 3)]
      (if (> i lim)
        res
        (if (aget refs i)
          (if (<= i llim)
            (do (loop [j (* i 2)]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j i)))))
                (recur (* res (/ (- (* i i) 1) (* i i)))
                       (+ i 2)))
            (recur (* res (/ (- (* i i) 1) (* i i)))
                   (+ i 2)))
          (recur res (+ i 2)))))))

(defn ^long sol159
  [^long lim]
  (let [refs (int-array (map #(let [x (rem % 9)]
                               (if (== x 0) 9 x))
                             (range lim)))
        llim (int (Math/sqrt lim))]
    (loop [i (int 2) res (long 0)]
      (if (>= i lim)
        res
        (let [ci (aget refs i)]
          (if (<= i llim)
            (do (loop [j (int (* i i))]
                  (if (>= j lim)
                    nil
                    (let [r (quot j i)
                          cur (aget refs j)
                          posj (+ ci (aget refs r))]
                      (do (when (> posj cur)
                            (aset refs j posj))
                          (recur (+ j i))))))
                (recur (+ i 1) (+ res ci)))
            (recur (+ i 1) (+ res ci))))))))

(def sum-two-tolol
  (memoize
    (fn [remainder ^long pow]
      (cond
        (== pow 0) (cond (> remainder 2) 0
                         (== remainder 2) 1
                         (== remainder 0) 1
                         :else 0)
        (== 0 remainder) 1
        (< remainder 0) 0
        :else (+' (sum-two-tolol (- remainder (expt 2 pow))
                                 (- pow 1))
                  (sum-two-tolol (- remainder (* 2 (expt 2 pow)))
                                 (- pow 1))
                  (sum-two-tolol remainder (- pow 1)))))))

(defn last-pow2
  [target]
  (->> (range)
       (take-while #(<= (expt 2 %) target))
       last))

(def sum-two
  (memoize
    (fn [remainder free]
      (cond
        (== 0 remainder) 1
        (< remainder 0) 0
        :else (let [lp (last-pow2 remainder)]
                (cond
                  (== lp 1) (if free 2 1)
                  :else (let [lp (last-pow2 remainder)]
                          (+ (sum-two (- remainder (expt 2 lp)) free)
                             (sum-two (- remainder (expt 2 (- lp 1))) false)))))))))


(def sum-two-alfa
  "Do this in binary style, but instead of doing the actual binary,
  it simply carry the information about the next smaller power"
  (memoize
    (fn [n]
      (if (zero? n)
        [1 0]
        (let [[a b] (sum-two-alfa (quot n 2))]
          (if (even? n)
            [(+ a b) b]
            [a (+ a b)]))))))

(def sol
  (memoize
    (fn [i cl lim cur]
      (let [nums (map #(conj cur %) (if (== cl 1) [:a :o] [:a :l :o]))
            ncur (->> nums
                      (map #(vec (rest %)))
                      (filter #(not-every? #{:a} %)))]
        (if (== i (- lim 1))
          (count ncur)
          (->> ncur
               (map #(recur (+ i 1)
                            (if (== cl 1) cl (if (some #{:l} %) 1 0))
                            lim %))
               (reduce +)))))))

(defn ^long tsol
  [^long lim]
  (->> (permutes [:a :l :o] 3)
       (filter #(<= (count (filter (fn [x] (= :l x)) %)) 1))
       (filter #(not-every? #{:a} %))
       (pmap #(sol 3 (if (some #{:l} %) 1 0) lim %))
       (reduce +)))











