(def rounds (-> (slurp "rounds.txt") string/trim scan-number))
(def stop (+ rounds 2))

(var pi 1.0)
(var x 1.0)

(for i 2 (+ stop 1)
  (set x (- x))
  (set pi (+ pi (/ x (- (* 2 i) 1)))))

(set pi (* pi 4.0))

(printf "%.16f" pi)
