;; This is a parallel/concurrent version of problem 84

(ns alfa.special.pp84
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

(def cc-cards (ref (shuffle (concat [:JAIL :GO] (repeat 14 0)))))

(def ch-cards (ref (shuffle (concat (repeat 6 0)
                                    [:GO :JAIL :C1 :E3 :H2 :R1]
                                    (repeat 2 :next-r)
                                    [:next-u -3]))))

(defn dice-4
  []
  (inc (rand-int 4)))

(defn land-on
  "Get the block representative of an integer representation of block"
  [init dices]
  (i2b (rem (+ dices (b2i init)) 40)))

(defn move
  "The move from init, the dice will be thrown randomly, and double indicates the current count of double"
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
  "The exact block a player land on"
  [land]
  (cond
    (some #{land} [:CC1 :CC2 :CC3])
    (let [card (first @cc-cards)]
      (do (dosync (ref-set cc-cards (concat (rest @cc-cards) [(first @cc-cards)])))
          (if (integer? card) (land-on land card) card)))
    (some #{land} [:CH1 :CH2 :CH3])
    (let [card (first @ch-cards)]
      (do (dosync (ref-set ch-cards (concat (rest @ch-cards) [(first @ch-cards)])))
          (cond
            (= -3 card) (block (i2b (- (b2i land) 3)))
            (integer? card) (land-on land card)
            (= card :next-r) (next-r land)
            (= card :next-u) (next-u land)
            :else card)))
    (= land :G2J) :JAIL
    :else land))

(def moves (ref {}))

(defn one-player
  "The movement of one player with lim as the number of sequential moves"
  [lim]
  (loop [i (int 1) pos :GO double 0]
    (when (<= i lim)
      (let [[npos1 ndouble] (move pos double)
            npos (block npos1)]
        (do (dosync (alter moves #(merge-with + % {npos 1})))
            (when (== 0 (rem i 5000)) (println (reduce + (vals @moves))))
            (recur (inc i) npos ndouble))))))

(defn play
  "The play with n as the number of players, and lim as the number of moves for each player"
  [^long n ^long lim]
  (do (pmap one-player (repeat n lim))
      (do (Thread/sleep 1800)
          (->> (sort-by val > @moves)
               (map #(let [[a b] %] [(b2i a) b]))))))









