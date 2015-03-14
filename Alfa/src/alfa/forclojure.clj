(ns alfa.forclojure)

(comment
  (defn f101
    ([sa sb] (f101 sa sb 0 0))
    ([sa sb ct xl]
       (if (empty? sa)
         (+ ct (count sb))
         (if (empty? sb)
           (+ ct (count sa))
           (if (zero? xl)
             (loop [[xa & xas] sa [xb & xbs] sb ra [] rb [] ctr ct]
               (if xa
                 (if xb
                   (if (= xa xb)
                     (recur xas xbs ra rb ctr)
                     (recur xas xbs (conj ra xa) (conj rb xb) ctr))
                   (f101 (concat ra [xa] xas) rb ctr 1))
                 (if xb
                   (f101 ra (concat rb [xb] xbs) ctr 1)
                   (f101 ra rb ctr 1))))
             (let [ca (count sa) cb (count sb)]
               (cond
                (= ca cb) ca
                (< ca cb) (f101 (cons (first sb) sa) sb (inc ct) 0)
                (> ca cb) (f101 (rest sa) sb (inc ct) 1)))))))))

(def test-cases
  [(f101 "kitten" "sitting")
   (f101 "closure" "clojure")
   (f101 "xyx" "xyyyx")
   (f101 "" "123456")
   (f101 "" "")
   (f101 "Clojure" "Clojure")])
