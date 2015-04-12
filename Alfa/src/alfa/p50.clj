(ns alfa.p50
	(:require
		[clojure.set :refer [union difference intersection subset?]]
		[clojure.core.reducers :as r]
		[clojure.string :as cs]
		[alfa.common :refer :all]))

(defn ^long sol1
	[^long lim]
	(let [[a b c] (pvalues (apply + (range 3 lim 3))
												 (apply + (range 5 lim 5))
												 (apply + (range 15 lim 15)))]
		(- (+ a b) c)))

(defn ^long sol1a
	[^long lim]
	(loop [i (int 1) res (int 0)]
		(if (>= i lim)
			res
			(if (or (== 0 (rem i 3))
							(== 0 (rem i 5)))
				(recur (+ i 1) (+ i res))
				(recur (+ i 1) res)))))

(defn ^long sol5
	[^long lim]
	(let [refs (int-array (range (+ lim 1)))]
		(loop [i (int 2) res (int 1)]
			(if (> i lim)
				res
				(let [tmpi (aget refs i)]
					(do (loop [j (int (+ i 1))]
								(if (> j lim)
									nil
									(let [tmpj (aget refs j)]
										(if (== 0 (rem tmpj tmpi))
											(do (aset refs j (quot tmpj tmpi))
													(recur (+ j 1)))
											(recur (+ j 1))))))
							(recur (+ i 1) (* res tmpi))))))))

(defn ^long sol9
	[^long lim]
	(loop [m (int 2)]
		(if (> m (quot lim 2))
			nil
			(let [msqr (* m m)]
				(if-let [res (loop [n (int 1)]
											 (if (>= n m)
												 nil
												 (let [nsqr (* n n)
															 a (- msqr nsqr)
															 b (* 2 m n)
															 c (+ msqr nsqr)
															 peri (+ a b c)]
													 (if (> peri lim)
														 nil
														 (if (== 0 (rem lim peri))
															 (* (expt (quot lim peri) 3) (* a b c))
															 (recur (+ n 1)))))))]
					res
					(recur (+ m 1)))))))











