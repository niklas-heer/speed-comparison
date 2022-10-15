(ns leibniz)

;; https://codereview.stackexchange.com/a/19942
(defn calc-pi-leibniz [terms]
  (* 4 (- (reduce + (map / (range 1.0 terms 4.0)))
          (reduce + (map / (range 3.0 terms 4.0))))))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(def rounds (parse-int (slurp "rounds.txt")))
(println (calc-pi-leibniz rounds))
