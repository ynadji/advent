(in-package :aoc2024)

(defun safe-report? (levels)
  (and (or (apply #'< levels)
           (apply #'> levels))
       (loop for (x y) on levels while y always (<= (abs (- x y)) 3))))

(defun safe-report-with-tolerance? (levels)
  (loop for i from 0 for x in levels
          thereis (safe-report? (remove x levels :start i :count 1))))

(defun count-safe-reports (input-file report-func)
  (loop for line in (uiop:read-file-lines input-file)
        for levels = (mapcar #'parse-integer (str:split " " line))
        count (funcall report-func levels)))

(defun day-02 ()
  (let ((f (fetch-day-input-file 2024 2)))
    (values (count-safe-reports f #'safe-report?)
            (count-safe-reports f #'safe-report-with-tolerance?))))
