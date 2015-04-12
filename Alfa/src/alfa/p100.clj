(ns alfa.p100
	(:require
		[clojure.set :refer [union difference intersection subset?]]
		[clojure.core.reducers :as r]
		[clojure.string :as cs]
		[alfa.common :refer :all]))

(defn sol52
	[^long start]
	(loop [i (int start)]
		(let [lst (map #(* i %) (range 2 7))]
			(if (every? #(= % (sort (numcol i)))
									(map #(sort (numcol %)) lst))
				i
				(recur (+ i 1))))))

(defn pascalim
	[^long lim ^long llim]
	(take lim (iterate #(vec (take-while (fn [x] (<= x llim)) (cons 1 (map +' % (conj (vec (rest %)) 0))))) [1 1])))

(defn sol53
	[^long lim]
	(->> (pascalim lim 1000000)
			 (drop 23)
			 (map butlast)
			 (map count)
			 (reduce +)
			 (* 2)
			 (- (+ 2 (triangle (+ 1 lim))) (triangle 24))))

(def bahan107
	(->> (cs/split-lines (slurp "resources/p107.txt"))
			 (map #(cs/split % #","))
			 (mapv #(mapv (fn [x] (if (= "-" x) 0 (read-string x))) %))))

(def limb (- (count bahan107) 1))

(def path
	(memoize
		(fn [a b]
			(let [tmp (get-in bahan107 [a b])]
				(if (pos? tmp)
					[tmp [a b]]
					(cond (== a (- b 1))
								(== b limb)
								:else (let [[ja jb] (path (+ a 1) b)]
												[(+ ja (get-in bahan107 [a (+ a 1)]))
												 (cons a jb)])))))))

(defn sol107
	[bahan]
	(let [matrix (into-array (map into-array bahan))
				lima (count matrix)
				limb (count (aget matrix 1))
				path (fn path [a b visits]
							 (let [tmp (get-in matrix [a b])]
								 (if (pos? tmp)
									 [tmp [a b]]
									 (let [[ja jb] (->> (range lima)
																			(remove visits)
																			(map #(path % b (conj visits %)))
																			(min-by first))]
										 [ja (cons a jb)]))))]
		(path 10 34 #{10 34})))

