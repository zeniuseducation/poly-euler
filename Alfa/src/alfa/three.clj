(ns alfa.three
	(:require
		[clojure.set :refer [union difference intersection subset?]]
		[clojure.core.reducers :as r]
		[clojure.string :refer [split-lines]]
		[alfa.common :refer :all]))

(comment
	(cb/defclient cdb {:bucket "primes"
										 :uris   ["http://127.0.0.1:8091/pools"]}))


(def cm (set! *unchecked-math* true))

(defn sum-sieve
	[^long lim]
	(let [refs (boolean-array (+ 1 lim) true)
				llim (int (Math/sqrt lim))]
		(loop [i (int 3) res (int 2)]
			(if (> i lim)
				res
				(if (aget refs i)
					(if (<= i llim)
						(do (loop [j (int (* i i))]
									(if (> j lim)
										nil
										(recur (do (aset refs j false)
															 (+ j (* i 2))))))
								(recur (+ i 2) (+ res i)))
						(recur (+ i 2) (+ res i)))
					(recur (+ i 2) res))))))

(comment
	(defn put-primes
		[^long lim]
		(let [refs (boolean-array (+ 1 lim) true)
					llim (int (Math/sqrt lim))]
			(loop [i (int 3)]
				(if (> i lim)
					nil
					(if (aget refs i)
						(if (<= i llim)
							(do (loop [j (int (* i i))]
										(if (> j lim)
											nil
											(recur (do (aset refs j false)
																 (+ j (* i 2))))))
									(recur (+ i 2)))
							(recur (do (set-json cdb (keyword (str "prime" i))
																	 {:number i})
												 (+ i 2))))
						(recur (+ i 2))))))))

(defn udivisors
	[n]
	(loop [i 2 res [1 n]]
		(if (> (* i i) n)
			(sort res)
			(recur (+ i 1)
						 (let [num (rem n i)]
							 (if (== 0 num)
								 (let [jum (quot n i)]
									 (if (== 1 (gcd jum i))
										 (conj res i jum)
										 res))
								 res))))))

(defn factmod
	[n modi]
	(loop [i 1 res 1]
		(if (> i n)
			res
			(recur (+ 1 i)
						 (rem (* i res) modi)))))

(defn logbase
	[base num]
	(/ (Math/log num)
		 (Math/log base)))

(defn ^long sol
	[^long nolim ^long lim ^long modi ^long target]
	(let [refs (boolean-array (+ 1 lim) true)
				divs (long-array (+ 1 lim) 0)
				llim (int (Math/sqrt lim))]
		(do (let [j2 (->> (range 1 (+ 1 (Math/floor (logbase 2 lim))))
											(map #(quot lim (expt 2 %)))
											(reduce +)
											(+ 1))]
					(aset divs 2 j2))
				(loop [i (int 3) res (int 1)]
					(if (> i lim)
						res
						(if (aget refs i)
							(do (let [j2 (->> (range 1 (+ 1 (Math/floor (logbase i lim))))
																(map #(quot lim (expt i %)))
																(reduce +)
																(+ 1))]
										(aset divs i j2))
									(if (<= i llim)
										(do (loop [j (* i i)]
													(if (> j lim)
														nil
														(do (aset refs j false)
																(recur (+ j (* i 2))))))
												(recur (+ i 2) (+ 1 res)))
										(recur (+ i 2) (+ res 1))))
							(recur (+ i 2) res))))
				(let [start (->> (range 2 (+ 1 nolim))
												 (filter #(aget refs %))
												 (map #(logbase 2 (aget divs %)))
												 (reduce +))]
					(loop [i (int nolim) res start]
						(if (>= res target)
							(factmod i modi)
							(let [nexi (loop [j (+ i 2)]
													 (if (aget refs j) j (recur (+ j 2))))]
								(recur nexi (+ res (logbase 2 (aget divs nexi)))))))))))

(defn repeating
	[^long n]
	(loop [i (expt 10 (Math/ceil (Math/log10 n)))
				 res #{} tmp [] ires #{}]
		(let [num (quot i n)
					numi (rem i n)]
			(if (some #{numi} res)
				(if (some #{numi} ires)
					[n (count tmp)]
					(recur (* numi 10) res (conj tmp num) (conj ires numi)))
				(recur (* numi 10) (conj res numi) tmp ires)))))

(defn ^long tots
	[^long lim]
	(->> (range 1 lim)
			 (pmap repeating)
			 (max-by second)))

(defn sol500
	[^long start ^long lim ^long target ^long modi]
	(let [primes (sieve lim)
				refs (boolean-array (+ 1 lim) false)
				cdivs (fn [^long n ^long maxi]
								(->> (range 1 (+ 1 (Math/floor (logbase n maxi))))
										 (map #(quot maxi (expt n %)))
										 (reduce +)
										 (+ 1)))]
		(do (doseq [i primes]
					(aset refs i true))
				(loop [i (int start)]
					(let [num (loop [j 2 res 0]
											(if (> j i)
												res
												(if (aget refs j)
													(recur (+ j 1)
																 (+ res (logbase 2 (cdivs j i))))
													(recur (+ j 1)
																 res))))]
						(if (>= num target)
							(factmod i modi)
							(recur (+ i 1))))))))

;; extract variable -> a very cool thing to use

(defn sol
	[^long start ^long tar]
	(let [psqr? (fn [^long n]
								(if (== 1 n)
									false
									(let [num (StrictMath/sqrt n)]
										(== (long num) num))))
				sat? (fn [n]
							 (let [num (- (long (StrictMath/cbrt n)) 1)]
								 (loop [i num res (int 0)]
									 (if (< i 4)
										 (if (== 4 res) true false)
										 (let [ncube (expt i 3)]
											 (recur (- i 1)
															(if (psqr? (- n ncube)) (+ 1 res) res)))))))]
		(loop [i (long start) res []]
			(if (>= (count res) tar)
				(reduce + res)
				(let [numi (numcol i)
							rev (reverse numi)
							n2 (colnum (concat numi rev))
							n1 (colnum (concat (butlast numi) rev))]
					(if (sat? n1)
						(if (sat? n2)
							(recur (+ 1 i) (conj res n1 n2))
							(recur (+ i 1) (conj res n1)))
						(if (sat? n2)
							(recur (+ 1 i) (conj res n2))
							(recur (+ 1 i) res))))))))

(defn solp
	[^long start ^long tar]
	(let [psqr? (fn [^long n]
								(if (== 1 n)
									false
									(let [num (StrictMath/sqrt n)]
										(== (long num) num))))
				sat? (fn [n]
							 (let [num (- (long (StrictMath/cbrt n)) 1)]
								 (loop [i num res (int 0)]
									 (if (< i 4)
										 (if (== 4 res) true false)
										 (let [ncube (expt i 3)]
											 (recur (- i 1)
															(if (psqr? (- n ncube)) (+ 1 res) res)))))))]
		(->> (iterate inc start)
				 (r/mapcat #(let [numi (numcol %)
													rev (reverse numi)
													n2 (colnum (concat numi rev))
													n1 (colnum (concat (butlast numi) rev))]
										 (if (sat? n1)
											 (if (sat? n2) (vector n1 n2) (vector n1))
											 (if (sat? n2) (vector n2) nil))))
				 (r/take tar)
				 (r/fold +))))

(defn solb
	[^long lim-sqr ^long lim-cube]
	(let [palin? (fn [^long n]
								 (let [ncol (numcol n)]
									 (= ncol (reverse ncol))))]
		(->> (+ isqr jcube)
				 (for [i (range 2 (+ lim-sqr 1))
							 j (range 2 (+ lim-cube 1))
							 :let [isqr (* i i)
										 jcube (* j j j)]
							 :when (palin? (+ isqr jcube))])
				 distinct sort
				 (take 5))))

(defn sol
	[^long lim]
	(let [frogs (boolean-array [false true true true true
															false false true true true
															false true true false true false])
				refs (boolean-array (+ 1 lim) false)
				valp {true 2/3 false 1/3}
				valn {true 1/3 false 2/3}
				croaks (memoize
								 (fn croaks [^long i ^long n]
									 (if (== n 15)
										 (if (aget refs i)
											 (valp (aget frogs n))
											 (valn (aget frogs n)))
										 (cond
											 (== i 1) (* (valn (aget frogs n))
																	 (croaks (+ i 1) (+ n 1)))
											 (== i lim) (* (valn (aget frogs n))
																		 (croaks (- i 1) (+ n 1)))
											 :else (if (aget refs i)
															 (+ (* 1/2 (valp (aget frogs n))
																		 (croaks (+ i 1) (+ n 1)))
																	(* 1/2 (valp (aget frogs n))
																		 (croaks (- i 1) (+ n 1))))
															 (+ (* 1/2 (valn (aget frogs n))
																		 (croaks (+ i 1) (+ n 1)))
																	(* 1/2 (valn (aget frogs n))
																		 (croaks (- i 1) (+ n 1)))))))))]
		(do (doseq [i (sieve lim)]
					(aset refs i true))
				(->> (range 1 (+ 1 lim))
						 (pmap #(* (/ 1 lim) (croaks % 1)))
						 (reduce +)))))


(defn sol
	[n]
	(loop [i 156789]
		(let [num (/ i n)]
			(if (integer? num)
				num
				(recur (+ i 100000))))))

(defn ^boolean prime?
	[^long n]
	(loop [i (int 3)]
		(if (> (* i i) n)
			true
			(if (== 0 (rem n i))
				false
				(recur (+ i 2))))))

(defn solp
	[^long lim]
	(->> (range 3 lim 2)
			 (filter prime?)
			 (reduce +)))

(defn sol
	[^long lim]
	(let [primes (boolean-array (* 10 lim) true)
				rela (boolean-array (* 10 lim) false)
				checked (boolean-array (* 10 lim) false)
				tlim (+ lim 100)
				llim (int (Math/sqrt tlim))
				conn (fn conn [^long n]
							 (if (< n 10)
								 n
								 (let [ncol (numcol n)
											 bsc [(colnum (butlast ncol))
														(colnum (rest ncol))]
											 bhn (->> (loop [[x & xs] ncol i (int 0) res (distinct bsc)]
																	(if xs
																		(recur xs (+ i 1)
																					 (concat res (let [dep (take i ncol)
																														 blk (drop (+ i 1) ncol)]
																												 (map #(colnum (concat dep [%] blk))
																															(remove #{x} (range 10))))))
																		(concat res (map #(colnum (concat (butlast ncol) [%]))
																										 (remove #{x} [1 3 7 9])))))
																(filter #(and (not= % n)
																							(aget primes %)
																							(not (aget checked %)))))
											 nums (keep #(let [cn (conn %)]
																		(if cn
																			(vector % cn)
																			nil)) bhn)]
									 (if (empty? nums)
										 (do (aset checked n true)
												 nil)
										 (if (some #(and (< (second %) n)
																		 (< (first %) n)) nums)
											 (do (aset rela n true)
													 n)
											 (apply min (map #(apply max %) nums)))))))]
		(do (loop [i (int 3)]
					(if (> i llim)
						nil
						(if (aget primes i)
							(do (loop [j (int (* i i))]
										(if (> j tlim)
											nil
											(recur (do (aset primes j false)
																 (+ j (* i 2))))))
									(recur (+ i 2)))
							(recur (+ i 2)))))
				(loop [i (int 3) res (long 0)]
					(if (> i lim)
						res
						(do (conn i)
								(if (aget rela i)
									(recur (loop [j (+ i 2)]
													 (if (aget primes j)
														 j
														 (recur (+ j 2))))
												 res)
									(recur (loop [j (+ i 2)]
													 (if (aget primes j)
														 j
														 (recur (+ j 2))))
												 (+ res i)))))))))


(defn ^long small
	[^long n]
	(loop [i (int 1)]
		(if (every? #{0 1 2} (numcol (* n i)))
			i
			(recur (+ i 1)))))

(defn ^long sol
	[^long lim]
	(->> (range 1 (+ lim 1))
			 (remove #{9 99 999 9999})
			 (pmap small)
			 (reduce +)))

(defn solp
	[^long lim]
	(loop [i (int 2) res (permutes [:o :a] 2)]
		(if (== i lim)
			res
			(let [tmp (for [r res p [:o :a]
											:let [cur (conj r p)]
											:when (not-every? #{:a} (take-last 3 cur))]
									cur)]
				(recur (+ 1 i) tmp)))))

(defn sol
	[^long i ^long lim ^long cl ^long ca]
	(if (== i lim)
		(if (== cl 1)
			(if (== 2 ca) 1 2)
			(if (== 2 ca) 2 3))
		(if (== cl 1)
			(if (== 2 ca)
				(sol (+ i 1) lim cl 2)
				(if (== 1 ca)
					(+ (sol (+ i 1) lim cl 2)
						 (sol (+ i 1) lim cl 1))
					(+ (sol (+ 1 i) lim cl 0)
						 (sol (+ 1 i) lim cl 1)
						 (sol (+ 1 i) lim cl 2))))
			(if (== 2 ca)
				(+ (sol (+ i 1) lim cl 2)
					 (sol (+ i 1) lim 1 2))
				(if (== 1 ca)
					(+ (sol (+ i 1) lim cl 2)
						 (sol (+ i 1) lim cl 1)
						 (sol (+ i 1) lim 1 2)
						 (sol (+ i 1) lim 1 1))
					(+ (sol (+ 1 i) lim cl 0)
						 (sol (+ 1 i) lim cl 1)
						 (sol (+ 1 i) lim cl 2)
						 (sol (+ 1 i) lim 1 0)
						 (sol (+ 1 i) lim 1 1)
						 (sol (+ 1 i) lim 1 2)))))))

(def sol
	(memoize
		(fn [i cl lim cur]
			(let [nums (map #(conj cur %) (if (== cl 1) [:a :o] [:a :l :o]))
						ncur (->> nums
											(map #(vec (rest %)))
											(filter #(if (< (count %) 3)
																true
																(not-every? #{:a} %))))]
				(if (== i lim)
					ncur
					(->> ncur
							 (map #(sol (+ i 1)
													(if (== cl 1) cl (if (some #{:l} %) 1 0))
													lim %))
							 (reduce +)))))))

(defn ^long tsol
	[^long lim]
	(->> (pmap #(sol 1 lim %) [[:a] [:l] [:o]])
			 (reduce +)))
















