(in-package :aoc2025)

(defparameter test-input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(defun day-07-part-1 (input-file)
  (multiple-value-bind (grid starts) (read-grid input-file :starts? (lambda (c) (char= c #\S)))
    (let ((tachyon-columns (list (cdar starts)))
          (num-splits 0)
          (max-col (array-dimension grid 1)))
      (loop for i from 1 below (array-dimension grid 0)
            do (loop for j in tachyon-columns
                     do (when (char= #\^ (aref grid i j))
                          ;;(format t "found ^ at ~a ~a~%" i j)
                          (incf num-splits)
                          (when (< j max-col)
                            (pushnew (1+ j) tachyon-columns))
                          (when (not (minusp (1- j)))
                            (pushnew (1- j) tachyon-columns))
                          (setf tachyon-columns (delete j tachyon-columns)))))
      num-splits)))

(defstruct beam col weight)

(defun day-07-part-2 (input-file)
  (multiple-value-bind (grid starts) (read-grid input-file :starts? (lambda (c) (char= c #\S)))
    (let ((tachyon-beams (list (make-beam :col (cdar starts) :weight 1)))
          (num-splits 0)
          (max-col (array-dimension grid 1)))
      (loop for i from 1 below (array-dimension grid 0)
            do (loop for beam in tachyon-beams
                     for j = (beam-col beam)
                     do ;;(format t "~a: ~a~%" i tachyon-beams)
                        (when (char= #\^ (aref grid i j))
                          ;;(format t "found ^ at ~a ~a~%" i j)
                          (incf num-splits)
                          (when (< j max-col)
                            (ax:if-let ((other-beam (find (1+ j) tachyon-beams :key #'beam-col)))
                              (incf (beam-weight other-beam) (beam-weight beam))
                              (push (make-beam :col (1+ j) :weight (beam-weight beam)) tachyon-beams)))
                          (when (not (minusp (1- j)))
                            (ax:if-let ((other-beam (find (1- j) tachyon-beams :key #'beam-col)))
                              (incf (beam-weight other-beam) (beam-weight beam))
                              (push (make-beam :col (1- j) :weight (beam-weight beam)) tachyon-beams)))
                          (setf tachyon-beams (delete beam tachyon-beams)))))
      (loop for beam in tachyon-beams sum (beam-weight beam)))))

(defun day-07 ()
  (let ((f (fetch-day-input-file 2025 7)))
    (values (day-07-part-1 f)
            (day-07-part-2 f))))
