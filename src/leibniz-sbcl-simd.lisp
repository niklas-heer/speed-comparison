#-sbcl (error "SBCL only!")

(declaim (optimize (speed 3) (safety 0) (debug 0))) ; only speed matters

(require 'sb-simd)

;; adapted from leibnix_avx2.cpp.
(defun leibniz (rounds)
  (declare (optimize (speed 3) (safety 0) (debug 0))
   (fixnum rounds))
  (incf rounds 2) ; do this outside the loop
  (let ((unroll 4)
        (x (sb-simd-avx2:make-f64.4 -1.0 1.0 -1.0 1.0))
        (den (sb-simd-avx2:make-f64.4 0.0 0.0 0.0 0.0))
        (inc (sb-simd-avx2:make-f64.4 4.0 4.0 4.0 4.0))
        (two (sb-simd-avx2:make-f64.4 2.0 2.0 2.0 2.0))
        (mone (sb-simd-avx2:make-f64.4 -1.0 -1.0 -1.0 -1.0))
        (ivec (sb-simd-avx2:make-f64.4 2.0 3.0 4.0 5.0))
        (pivec (sb-simd-avx2:make-f64.4 0.0 0.0 0.0 0.0)))
    (let ((vec-end (- rounds (mod rounds unroll))))
      (loop for i from 2 below vec-end by unroll do
        ;; compute den = (2 * i - 1)
        (setf den (sb-simd-avx2:f64.4+ (sb-simd-avx2:f64.4* two ivec) mone)
              ;; increment ivec, so ivec +=inc
              ivec (sb-simd-avx2:f64.4+ ivec inc)
              ;; compute partial sums
              pivec (sb-simd-avx2:f64.4+ pivec (sb-simd-avx2:f64.4/ x den))))

      ;; gather the partial sums
      (let ((underscore-pi (+ 1.0d0 (sb-simd-avx:f64.4-horizontal+ pivec))))
        ;; now the wind-down loop
        (loop with underscore-x = 1.0d0 for i from vec-end below rounds do
          (setf underscore-x (- underscore-x)
                underscore-pi (+ underscore-pi (* underscore-x (/ (- (* 2.0 i) 1.0))))))
        (* underscore-pi 4.0d0)))))

#-swank
(with-open-file (in "rounds.txt")
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((*read-default-float-format* 'double-float)
        (n (parse-integer (read-line in))))
    (princ (leibniz n))
    (fresh-line)))
