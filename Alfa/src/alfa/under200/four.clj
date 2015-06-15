(ns alfa.under200.four
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all]
   [clojure.string :as cs]))

(defn palin?
  [^long i]
  (let [n (numcol i)]
    (= n (reverse n))))

(def maxi
  (memoize
   (fn [^long i ^long j]
     (if (or (< i 950) (< j 950))
       0
       (let [res (let [resi (* i j)]
                   (if (palin? resi) resi 0))]
         (max res (maxi i (- j 1)) (maxi (- i 1) (- i 2))))))))
