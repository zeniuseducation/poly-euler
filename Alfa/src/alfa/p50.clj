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

(def bahan18
	(->> (cs/split-lines (slurp "resources/p18.txt"))
			 (map #(cs/split % #" "))
			 (mapv #(mapv (fn [[x & xs :as n]]
											(if (= \0 x)
												(read-string (apply str xs))
												(read-string n))) %))))

(def bahan67
	(->> (cs/split-lines (slurp "resources/p67.txt"))
			 (map #(cs/split % #" "))
			 (mapv #(mapv (fn [[x & xs :as n]]
											(if (= \0 x)
												(read-string (apply str xs))
												(read-string n))) %))))

(def sol18
	(memoize
		(fn [a b]
			(if (== a 14)
				(get-in bahan18 [a b])
				(if (> b a)
					0
					(+ (get-in bahan18 [a b])
						 (max (sol18 (+ a 1) b)
									(sol18 (+ a 1) (+ b 1)))))))))

(def sol67
	(memoize
		(fn [a b]
			(if (== a 99)
				(get-in bahan67 [a b])
				(if (> b a)
					0
					(+ (get-in bahan67 [a b])
						 (max (sol67 (+ a 1) b)
									(sol67 (+ a 1) (+ b 1)))))))))

(defn lazy-sieve
	([^long lim]
	 (->> (iterate #(+ % 2) 5)
				(lazy-sieve 3)
				(cons 2)
				(take-while #(< % lim))))
	([a xs]
	 (->> (rest xs)
				(filter #(not= 0 (rem % a)))
				(lazy-sieve (first xs))
				(lazy-seq)
				(cons a))))

(defn collatz
	([^long n]
		(if (== n 1)
			1
			(collatz n 0)))
	([^long n ^long res]
		(if (== n 1)
			(+ 1 res)
			(if (even? n)
				(collatz (quot n 2) (+ res 1))
				(collatz (+ 1 (* 3 n)) (+ res 1))))))

(defn sol14
	[^long lim]
	(loop [i (int 500001) res (int 0) resn (int 0)]
		(if (> i lim)
			resn
			(let [tmp (collatz i)]
				(if (> tmp res)
					(recur (+ i 2) tmp i)
					(recur (+ i 2) res resn))))))

(defn sol28
	[^long lim]
	(let [spirals (fn [^long n]
									(reduce + (take 4 (iterate #(- % (- n 1)) (* n n)))))]
		(+ 1 (reduce + (map spirals (range 3 (+ lim 1) 2))))))
































