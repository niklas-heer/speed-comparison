#lang racket/base

(require racket/file
         racket/flonum
         racket/unsafe/ops)

(define rounds (file->value "rounds.txt"))

(let ([stop (unsafe-fx+ rounds 2)])
  (let loop ([i 2]
             [x -1.0]
             [pie 1.0])
    (if (unsafe-fx> i stop)
        (displayln (real->decimal-string (unsafe-fl* pie 4.0) 16))
        (loop (unsafe-fx+ i 1)
              (unsafe-fl- 0.0 x)
              (unsafe-fl+ pie (unsafe-fl/ x (unsafe-fl- (unsafe-fl* 2.0 (unsafe-fx->fl i)) 1.0)))))))
