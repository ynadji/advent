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

(defun parse-dial-rotations (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        collect (cons (char line 0) (parse-integer (subseq line 1)))))

(defun day-01-part-1 (input-file)
  (let ((rotations (parse-dial-rotations input-file))
        (value 50))
    (flet ((rotation-value (dir rotation)
             (if (eq dir #\L)
                 (- rotation)
                 rotation)))
     (loop for (dir . rotation) in rotations
           do (setf value (mod (+ value (rotation-value dir rotation)) 100))
           count (zerop value)))))

(defun day-01-part-2 (input-file)
  (let ((rotations (parse-dial-rotations input-file))
        (value 50))
    (loop for (dir . rotation) in rotations
          sum (loop repeat rotation
                    if (eq dir #\R)
                      do (setf value (mod (1+ value) 100))
                    else
                      do (setf value (mod (1- value) 100))
                    count (zerop value)))))

(defun day-01 ()
  (let ((f (fetch-day-input-file 2025 1)))
    (values (day-01-part-1 f)
            (day-01-part-2 f))))
