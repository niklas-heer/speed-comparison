(module* three #f
  (#%declare #:unsafe)
  (require (rename-in racket/unsafe/ops
                      [unsafe-fl- fl-]
                      [unsafe-fl+ fl+]
                      [unsafe-fl/ fl/]
                      [unsafe-fl* fl*])
           #;racket/flonum)
  (time
   (let ()
     (define pi
       (for/fold ([pi 1.0] [x 1.0] #:result (* pi 4.0))
                 ([i (in-range 2.0 (+ rounds 2.0))])
         (let ([x (fl- x)])
           (values (fl+ pi (fl/ x (fl- (fl* 2.0 i) 1.0))) x))))
     (displayln (list 'three (real->decimal-string pi 16))))))
