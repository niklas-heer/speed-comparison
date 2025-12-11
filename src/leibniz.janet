(def rounds (-> (slurp "rounds.txt") string/trim scan-number))
(def stop (+ rounds 2.0))

(var pi 1.0)
(var x 1.0)
(var i 2.0)

(while (<= i stop)
  (set x (- x))
  (set pi (+ pi (/ x (- (* 2.0 i) 1.0))))
  (set i (+ i 1.0)))

(set pi (* pi 4.0))

(printf "%.16f" pi)
