(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun leibniz (rounds)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (fixnum rounds))
  (let ((ans 1.0d0)
        (sig 1.0d0)
        (end (ash (1+ rounds) 1)))
    (declare (double-float ans sig)
             (fixnum end))
    (loop for i of-type fixnum from 3 below end by 2
          do (setf sig (- sig))
             (incf ans (/ sig i)))
    (* ans 4)))

#-swank
(with-open-file (in "rounds.txt")
  (declare (optimize (speed 3) (safety 0) (debug 0))) ; in the spirit of the thing..
  (let ((*read-default-float-format* 'double-float)
        (n (parse-integer (read-line in))))
    (princ (leibniz n))
    (fresh-line)))
