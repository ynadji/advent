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

(setf lparallel:*kernel* (lparallel:make-kernel 8))

(defun guard-duty (grid start)
  (declare (type (simple-array standard-char (* *)) grid))
  (let ((direction :north)
        (visited (make-hash-table :test #'equal :size 1024)))
    (loop with pos = start while pos
          for next-pos = (safe-advance direction pos grid)
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
  (multiple-value-bind (grid starts) (read-grid input-file :starts? (lambda (c) (char= c #\^)))
    (length (guard-duty grid (first starts)))))

(defun day-06-part-2 (input-file)
  (multiple-value-bind (grid starts) (read-grid input-file :starts? (lambda (c) (char= c #\^)))
    (declare (type (simple-array standard-char (* *)) grid))
    (labels ((find-cycle (pos grid)
               (declare (type (simple-array standard-char (* *)) grid))
               (destructuring-bind (i . j) pos
                 (when (char= (aref grid i j) #\.)
                   (setf (aref grid i j) #\#)
                   (when (eq :cycle (guard-duty grid (first starts)))
                     1)))))
      (let ((visited (guard-duty grid (first starts))))
        (apply #'+ (remove nil (lparallel:pmapcar #'find-cycle
                                                  visited
                                                  (loop repeat (length visited) collect (ax:copy-array grid)))))))))

(defun day-06 ()
  (let ((f (fetch-day-input-file 2024 6)))
    (values (day-06-part-1 f)
            (day-06-part-2 f))))
