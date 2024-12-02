(in-package :aoc2019)

(defun mass->fuel (mass)
  (- (floor (/ mass 3)) 2))

(defun day-01-part-1 (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        for mass = (parse-integer line)
        sum (mass->fuel mass)))

(defun day-01-part-2 (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        for mass = (parse-integer line)
        sum (loop for fuel = (mass->fuel mass) then (mass->fuel fuel)
                  while (plusp fuel)
                  sum fuel)))

(defun day-01 ()
  (let ((f (fetch-day-input-file 2019 1)))
    (values (day-01-part-1 f)
            (day-01-part-2 f))))
