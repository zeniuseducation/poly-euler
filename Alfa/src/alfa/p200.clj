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
