(ns alfa.beta.two)

(defn ^longs jumfak
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        faks (int-array (+ lim 1) 1)]
    (loop [i (int 2)]
      (if (> i llim)
        (filterv #(> (aget faks %) %) (range 12 (+ lim 1)))
        (do (let [isqr (* i i)]
              (do (aset faks isqr (+ (aget faks isqr) i))
                (loop [j (int (+ isqr i))]
                  (when (<= j lim)
                    (aset faks j (+ (aget faks j) i (quot j i)))
                    (recur (+ j i))))))
          (recur (+ i 1)))))))

(defn ^long sol23
  [^long lim]
  (let [abuns (jumfak lim)
        ctr (count abuns)
        rabuns (int-array abuns)
        refs (boolean-array (+ lim 1) false)
        hlim (quot lim 2)]
    (loop [i (int 0)]
      (let [iref (aget rabuns i)]
        (if (> iref hlim)
          (- (quot (* lim (+ lim 1)) 2)
             (transduce
              (filter #(aget refs %))
              + (range 12 (+ lim 1))))
          (do (loop [j (int i)]
                (let [jref (aget rabuns j)]
                  (when (<= (+ iref jref) lim)
                    (aset refs (+ iref jref) true)
                    (recur (+ j 1)))))
            (recur (+ i 1))))))))

(time (sol23 28123))

(dotimes [i 10]
  (time (sol23 28123)))

(time (jumfak 100))
