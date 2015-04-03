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

