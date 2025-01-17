(in-package :aoc2016)

(defun parse-bunny-turns (input-file)
  (let ((s (uiop:read-file-string input-file)))
    (values (mapcar (lambda (s) (char s 0)) (str:split ", " s))
            (string-to-num-list s))))

(defun day-01% (input-file)
  (labels ((distance-from-origin (final-pos)
             (+ (abs (car final-pos))
                (abs (cdr final-pos)))))
    (multiple-value-bind (turns distances) (parse-bunny-turns input-file)
      (loop with final-pos = (cons 0 0) with visited-twice with visited = (list (cons 0 0))
            with direction = :north
            for turn in turns for dist in distances
            do (setf direction (ecase turn
                                 (#\R (90-clockwise-direction direction))
                                 (#\L (90-counter-clockwise-direction direction))))
               ;; just increment by 1 so we maintain the trail of all visited locations.
               (loop repeat dist with pos-inc = (direction->delta direction)
                     do (setf final-pos (pos+ final-pos pos-inc))
                     when (and (not visited-twice)
                               (member final-pos visited :test #'equal))
                       do (setf visited-twice final-pos)
                     do (push final-pos visited))
            finally (return (values (distance-from-origin final-pos)
                                    (distance-from-origin visited-twice)))))))

(defun day-01 ()
  (let ((f (fetch-day-input-file 2016 1)))
    (day-01% f)))
