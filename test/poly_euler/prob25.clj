(ns poly-euler.test.prob25
  (:require [expectations :refer :all]
            [poly-euler.prob25 :refer :all]))

(expect (euler25 (expt 10 999) [1 1] 2)
        (euler25a (expt 10 999) [1 1] 2))

