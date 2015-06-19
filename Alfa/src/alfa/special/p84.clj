(ns alfa.special.p84
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

(def board [:GO :A1 :CC1 :A2 :T1 :R1 :B1 :CH1 :B2 :B3
            :JAIL :C1 :U1 :C2 :C3 :R2 :D1 :CC2 :D2 :D3
            :FP :E1 :CH2 :E2 :E3 :R3 :F1 :F2 :U2 :F3
            :G2J :G1 :G2 :CC3 :G3 :R4 :CH3 :H1 :T2 :H2])

(def b2i (zipmap board (range)))

(def i2b (zipmap (range) board))


(def cc-cards (atom (shuffle (concat [:JAIL :GO] (repeat 14 0)))))

(def ch-cards (atom (shuffle (concat (repeat 6 0)
                                     [:GO :JAIL :C1 :E3 :H2 :R1]
                                     (repeat 2 :next-r)
                                     [:next-u -3]))))

(defn dice-4
  []
  (inc (rand-int 4)))

(defn land-on
  [init dices]
  (i2b (rem (+ dices (b2i init)) 40)))

(defn move
  [init double]
  (let [d1 (dice-4) d2 (dice-4) ds (+ d1 d2)]
    (if (== d1 d2)
      (if (<= 0 double 1)
        [(land-on init ds) (inc double)]
        [:JAIL 0])
      [(land-on init ds) 0])))

(defn next-r
  [land]
  (first (filter #{:R1 :R2 :R3 :R4} (drop (inc (b2i land)) (cycle board)))))

(defn next-u
  [land]
  (first (filter #{:U1 :U2} (drop (inc (b2i land)) (cycle board)))))

(defn block
  [land]
  (cond
    (some #{land} [:CC1 :CC2 :CC3])
    (let [card (first @cc-cards)]
      (do (reset! cc-cards (concat (rest @cc-cards) [(first @cc-cards)]))
          (if (integer? card) (land-on land card) card)))
    (some #{land} [:CH1 :CH2 :CH3])
    (let [card (first @ch-cards)]
      (do (reset! ch-cards (concat (rest @ch-cards) [(first @ch-cards)]))
          (cond
            (= -3 card) (block (i2b (- (b2i land) 3)))
            (integer? card) (land-on land card)
            (= card :next-r) (next-r land)
            (= card :next-u) (next-u land)
            :else card)))
    (= land :G2J) :JAIL
    :else land))

(defn play
  [lim]
  (loop [i (int 0) pos :GO double 0 moves {}]
    (if (> i lim)
      (map #(let [[a b] %]
             [(b2i a) b]) (sort-by val > moves))
      (let [[npos1 ndouble] (move pos double)
            npos (block npos1)]
        (recur (inc i) npos ndouble (merge-with + moves {npos 1}))))))







