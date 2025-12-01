(in-package :aoc2025)

(defparameter test-input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defun day-01% (input-file)
  (flet ((parse-dial-rotations (input-file)
           (loop for line in (uiop:read-file-lines input-file)
                 collect (if (char= #\L (char line 0))
                             (- (parse-integer (subseq line 1)))
                             (parse-integer (subseq line 1))))))
    (let ((dial 50) (part1 0) (part2 0))
      (loop for rotation in (parse-dial-rotations input-file)
            for new-dial = (+ dial rotation)
            do (incf part2 (floor (abs new-dial) 100))
               (when (and (plusp dial) (not (plusp new-dial)))
                 (incf part2))
               (setf dial (mod new-dial 100))
               (when (zerop dial)
                 (incf part1))
            finally (return (values part1 part2))))))

(defun day-01 ()
  (let ((f (fetch-day-input-file 2025 1)))
    (day-01% f)))
