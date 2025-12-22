(import (chezscheme))

(define rounds (with-input-from-file "rounds.txt" read))

(let ([stop (fx+ rounds 2)])
  (let loop ([i 2]
             [x -1.0]
             [pie 1.0])
    (if (fx> i stop)
        (display (format "~,16f\n" (fl* pie 4.0)))
        (loop (fx+ i 1)
              (fl- 0.0 x)
              (fl+ pie (fl/ x (fl- (fl* 2.0 (fixnum->flonum i)) 1.0)))))))
