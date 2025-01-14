(in-package :aoc2015)

(defun next-code (code)
  (declare (optimize (speed 3))
           (type fixnum code))
  (mod (the fixnum (* 252533 code)) 33554393))

(defun triangular-number (n)
  (/ (* n (1+ n)) 2))

(defun row-at-col-1 (row col)
  (+ row (- col 1)))

(defun find-code-at-index (row col)
  (loop repeat (+ (1- col) (triangular-number (1- (row-at-col-1 row col))))
        with code = 20151125
        do (setf code (next-code code))
        finally (return code)))

(defun day-25-part-1 (input-file)
  (destructuring-bind (row col) (string-to-num-list (uiop:read-file-string input-file))
    (find-code-at-index row col)))

(defun day-25-part-2 (input-file) (progn input-file -1))

(defun day-25 ()
  (let ((f (fetch-day-input-file 2015 25)))
    (values (day-25-part-1 f)
            (day-25-part-2 f))))
