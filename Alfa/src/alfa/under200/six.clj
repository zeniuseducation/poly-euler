(ns alfa.under200.six
  (:require
   [clojure.set :as st]
   [clojure.string :as cs]))

(defn mymax [& xs]
  (let [f (fn [a b] (if (> a b) a b))]
    (reduce f xs)))




