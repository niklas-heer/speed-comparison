(ns leibniz)

(set! *unchecked-math* :warn-on-boxed)

(defn calc-pi-leibniz
  "Translation of Java solution to Clojure - uses doubles throughout to avoid long/double mixing"
  ^double
  [^long rounds]
  (let [end (double (+ 2 rounds))]
    (loop [i 2.0 x 1.0 pi 1.0]
      (if (== i end)
        (* 4.0 pi)
        (let [x (- x)]
          (recur (inc i) x (+ pi (/ x (dec (* 2.0 i))))))))))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(def rounds (parse-int (slurp "rounds.txt")))
(println (calc-pi-leibniz rounds))
