(in-package :aoc2016)

(defparameter test-input "ADVENT
A(1x5)BC
(3x3)XYZ
A(2x2)BCD(2x2)EFG
(6x1)(1x3)A
X(8x2)(3x3)ABCY")

;; works with the test cases but wrong answer :(
(defun decompressed-length (s)
  (labels ((parse-marker (s start)
             (cl-ppcre:register-groups-bind ((#'parse-integer x y)) ("\\((\\d+)x(\\d+)\\)" s :start start)
               (values x y))))
    (loop for c across s with total-length = 0 with in-marker? = nil
          with marker-length = 0 with maybe-add = 0 for i from 0 do
            (cond ((and (char= #\( c) (zerop marker-length))
                   (setf in-marker? t)
                   (multiple-value-bind (num-chars num-repeats) (parse-marker s i)
                     (setf marker-length num-chars)
                     (setf maybe-add (- (* num-chars num-repeats) num-chars))))
                  (in-marker? (when (char= #\) c)
                                (setf in-marker? nil)))
                  ((and (plusp marker-length) (char/= #\) c))
                   (decf marker-length)
                   (incf total-length)
                   (when (zerop marker-length)
                     (incf total-length maybe-add)
                     (setf maybe-add 0)))
                  (t (incf total-length)))
          finally (return total-length))))

(defun day-09-part-1 (input-file)
  (let ((s (uiop:read-file-string input-file)))
    (decompressed-length (str:trim s))))

(defun day-09-part-2 (input-file) (progn input-file -1))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2016 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
