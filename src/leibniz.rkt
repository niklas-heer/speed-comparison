#lang racket/base

(require racket/file
         racket/flonum)

(define rounds (file->value "rounds.txt"))

; use "pie" instead of "pi" to avoid confusion with racket's included "pi"
(let ([stop (fl+ (exact->inexact rounds) 2.0)])
  (let loop ([x (fl+ -1.0)]
             [pie (fl+ 1.0)]
             [i 2.0])
    (if (fl>= i stop)
        (displayln (real->decimal-string (* pie 4.0) 16))
        (loop (fl- x) (fl+ pie (fl/ x (fl- (fl* 2.0 i) 1.0))) (fl+ i 1.0)))))
