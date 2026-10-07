(in-package :aoc2017)

(defun max-min-diff (nums)
  (- (apply #'max nums) (apply #'min nums)))

(defun evenly-divided (nums)
  (loop for x in nums do
    (loop for y in nums for quotient = (/ x y)
          when (typep quotient '(integer 2 *))
            do (return-from evenly-divided quotient))))

(defun day-02% (input-file fun)
  (->> input-file uiop:read-file-lines (mapcar #'string-to-num-list) (mapcar fun) (reduce #'+)))

(defun day-02 ()
  (let ((f (fetch-day-input-file 2017 2)))
    (values (day-02% f #'max-min-diff)
            (day-02% f #'evenly-divided))))
