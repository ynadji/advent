(in-package :aoc2024)

(defparameter test-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(declaim (optimize (speed 3)))

(defun guard-duty (grid start)
  (declare (type (simple-array standard-char (* *)) grid))
  (let ((visited (make-hash-table :test #'equal))
        (direction :north))
    (loop with pos = start while pos
          for next-pos = (first (2d-neighbors grid pos :wanted-directions (list direction)))
          for next-char = (when next-pos (aref grid (car next-pos) (cdr next-pos)))
          if (gethash (cons direction pos) visited)
            do (return-from guard-duty (values :cycle (cons direction pos)))
          else
            do (ecase next-char
                 ((#\. #\^ nil)
                  (setf (gethash (cons direction pos) visited) t
                        pos next-pos))
                 (#\# (setf direction (90-clockwise-direction direction)))))
    (remove-duplicates (mapcar #'cdr (ax:hash-table-keys visited)) :test #'equal)))

(defun day-06-part-1 (input-file)
  (multiple-value-bind (grid start) (read-grid input-file #\^)
    (length (guard-duty grid start))))

(defun day-06-part-2 (input-file)
  (multiple-value-bind (grid start) (read-grid input-file #\^)
    (declare (type (simple-array standard-char (* *)) grid))
    (loop for (i . j) in (guard-duty grid start)
          for original-char = (aref grid i j)
          do (when (char= original-char #\.)
               (setf (aref grid i j) #\#))
          count (eq :cycle (guard-duty grid start))
          do (setf (aref grid i j) original-char))))

(defun day-06 ()
  (let ((f (fetch-day-input-file 2024 6)))
    (values (day-06-part-1 f)
            (day-06-part-2 f))))
