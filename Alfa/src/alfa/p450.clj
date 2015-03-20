(ns alfa.p450
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all])
  (:import (clojure.lang BigInt)))

(defn sol458
  [tar modi n]
  (let [korban (fact n)
        sisa n
        awali (modex n tar modi)]
    ))
