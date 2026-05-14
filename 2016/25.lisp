(in-package :aoc2016)

(defun day-25-part-1 (input-file)
  (loop for a from 0 for output = (day-12% input-file :a a)
        when output do (return a)))

(defun day-25-part-2 (input-file) (progn input-file -1))

(defun day-25 ()
  (let ((f (fetch-day-input-file 2016 25)))
    (values (day-25-part-1 f)
            (day-25-part-2 f))))
