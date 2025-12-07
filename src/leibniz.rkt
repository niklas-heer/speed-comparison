#lang racket/base
(require racket/string racket/file)

(define rounds (string->number (string-trim (file->string "rounds.txt"))))

(define pi 1.0)
(define x 1.0)

(for ([i (in-range 2 (+ rounds 2))])
  (set! x (- x))
  (set! pi (+ pi (/ x (- (* 2 i) 1)))))

(set! pi (* pi 4.0))
(displayln (real->decimal-string pi 16))
