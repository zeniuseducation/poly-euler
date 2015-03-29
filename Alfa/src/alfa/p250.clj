(ns alfa.p250
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all]))

(defn geo-arit
  [r n]
  (transduce
   (map #(* (- 900 (* 3 %))
            (Math/pow r (- % 1))))
   + (range 1 (+ n 1))))
