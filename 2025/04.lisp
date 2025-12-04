(in-package :aoc2025)

(defparameter test-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defun paper-roll? (grid i j)
  (char= #\@ (aref grid i j)))

(defun num-neighbor-rolls (grid i j)
  (flet ((paper-roll?% (pos)
           (paper-roll? grid (car pos) (cdr pos))))
    (let ((neighbors (2d-neighbors grid (cons i j) :wanted-directions *8-winds/deltas*)))
      (length (remove-if-not #'paper-roll?% neighbors)))))

(defun count-rolls (grid &optional remove?)
  (loop for i below (array-dimension grid 0)
        sum (loop for j below (array-dimension grid 1)
                  when (and (paper-roll? grid i j)
                            (< (num-neighbor-rolls grid i j) 4))
                    sum 1
                    and when remove?
                          do (setf (aref grid i j) #\.))))

(defun repeatedly-count-rolls (grid)
  (loop for num-removed = (count-rolls grid t)
        while (plusp num-removed)
        sum num-removed))

(defun day-04% (input-file count-fun)
  (funcall count-fun (read-grid input-file)))

(defun day-04 ()
  (let ((f (fetch-day-input-file 2025 4)))
    (values (day-04% f #'count-rolls)
            (day-04% f #'repeatedly-count-rolls))))
