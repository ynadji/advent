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

(defun dir-id (dir) (ecase dir (:north 1) (:east 2) (:south 4) (:west 8)))

(defun already-visited? (x direction)
  (declare (type fixnum x))
  (not (zerop (logand x (the fixnum (dir-id direction))))))

(defun update-visited (x direction)
  (declare (type fixnum x))
  (logior x (the fixnum (dir-id direction))))

(defun guard-duty (grid start &optional fake-block part2?)
  (declare (type (simple-array standard-char (* *)) grid))
  (let ((direction :north)
        (visited (make-array (array-dimensions grid) :element-type 'fixnum :initial-element 0)))
    (declare (type (simple-array fixnum (* *)) visited))
    (loop with pos = start while pos
          for dirpos = (cons direction pos)
          for next-pos = (safe-advance direction pos grid)
          for next-char = (when next-pos
                            (if (and fake-block (equal fake-block next-pos))
                                #\#
                                (paref grid next-pos)))
          if (already-visited? (paref visited pos) direction)
            do (return-from guard-duty (values :cycle dirpos))
          else
            do (ecase next-char
                 ((#\. #\^ nil)
                  (setf (paref visited pos) (update-visited (paref visited pos) direction)
                        pos next-pos))
                 (#\# (setf direction (90-clockwise-direction direction)))))
    (if part2?
        nil ; for part 2, we only need to know if there was a cycle or not.
        (remove-duplicates (loop for i below (array-dimension visited 0)
                                 append (loop for j below (array-dimension visited 1)
                                              unless (zerop (aref visited i j))
                                                collect (cons i j)))
                           :test #'equal))))

;; write guard-duty-fast here

(defun day-06-part-1 (input-file)
  (multiple-value-bind (grid starts) (read-grid input-file :starts? (lambda (c) (char= c #\^)))
    (length (guard-duty grid (first starts)))))

(defun day-06-part-2 (input-file)
  (multiple-value-bind (grid starts) (read-grid input-file :starts? (lambda (c) (char= c #\^)))
    (declare (type (simple-array standard-char (* *)) grid))
    (labels ((find-cycle (pos)
               (declare (type (simple-array standard-char (* *)) grid))
               (when (char= (paref grid pos) #\.)
                 (when (eq :cycle (guard-duty grid (first starts) pos t))
                   1))))
      (let ((visited (guard-duty grid (first starts))))
        (apply #'+ (remove nil (lparallel:pmapcar #'find-cycle visited)))))))

(defun day-06 ()
  (let ((f (fetch-day-input-file 2024 6)))
    (values (day-06-part-1 f)
            (day-06-part-2 f))))
