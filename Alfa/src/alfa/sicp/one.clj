(ns alfa.sicp.one)

;; This is an attempt to solve SICP problems in Clojure
;; each namespace represents the chapter in SICP

(def !! nth)
(def <> range)
(def ++ concat)
(def >> cons)
(def << conj)
(def $ #(reduce + %))
(def $$ #(reduce * %))
(def ! #($$ (range 1 (inc %))))
(def ** (fn expt [a m]
          (cond (== m 0) 1
                (== m 1) a
                :else (let [nex (expt a (quot m 2))]
                        (*' nex nex (if (even? m) 1 a))))))

(def % rem)
(def %% quot)
(def !< aget)
(def !> aset)
(def sqrt #(Math/sqrt %))
(def isqrt #(int (Math/sqrt %)))

(defn ^long sieve
  [^long lim]
  (let [primes (boolean-array (inc lim) true)
        llim (isqrt lim)]
    (loop [i (int 3) res (long 2)]
      (if (> i lim)
        res
        (if (aget primes i)
          (if (<= i llim)
            (do (loop [j (* i i)]
                  (when (<= j lim)
                    (aset primes j false)
                    (recur (+ j i i))))
                (recur (+ i 2) (+ res i)))
            (recur (+ i 2) (+ res i)))
          (recur (+ i 2) res))))))




