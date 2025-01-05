(in-package :aoc2015)

(defun day-01-part-1 (input-file)
  (loop for c across (uiop:read-file-string input-file)
        with floor = 0
        if (char= c #\()
          do (incf floor)
        else
          do (decf floor)
        finally (return floor)))

(defun day-01-part-2 (input-file)
  (loop for i from 1
        for c across (uiop:read-file-string input-file)
        with floor = 0
        if (char= c #\()
          do (incf floor)
        else
          do (decf floor)
        when (= floor -1)
          return i))

(defun day-01 ()
  (let ((f (fetch-day-input-file 2015 1)))
    (values (day-01-part-1 f)
            (day-01-part-2 f))))
