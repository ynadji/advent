(in-package :aoc2015)

(defun day-20% (input-file &optional part2)
  (let* ((num-presents (parse-integer (uiop:read-file-string input-file)))
         (num-presents/10 (/ num-presents 10))
         (houses (make-array num-presents/10 :element-type 'fixnum))
         (num-times (if part2 50 num-presents/10))
         (p-times (if part2 11 10)))
    (loop for i from 1 below num-presents/10
          do (loop for j from i by i below num-presents/10 repeat num-times
                   do (incf (aref houses j) (* p-times i))))
    (loop for i from 0 for n across houses
          when (>= n num-presents)
            return i)))

(defun day-20 ()
  (let ((f (fetch-day-input-file 2015 20)))
    (values (day-20% f)
            (day-20% f t))))
