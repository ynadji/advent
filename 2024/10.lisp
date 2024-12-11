(in-package :aoc2024)

(defparameter test-input-1 "0123
1234
8765
9876")

(defparameter test-input-2 "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defun read-trails (input-file)
  (let* ((lines (uiop:read-file-lines input-file))
         (rows (length lines))
         (cols (length (first lines)))
         (grid (make-array (list rows cols) :element-type 'fixnum))
         trailheads)
    (loop for row in lines for i from 0 do
      (loop for x across row for j from 0 do
        (setf (aref grid i j) (- (char-code x) 48))
        (when (= 0 (aref grid i j))
          (push (cons i j) trailheads))))
    (values grid trailheads)))

(defun hike (grid pos)
  (if (= 9 (aref grid (car pos) (cdr pos)))
      pos
      (let ((x (aref grid (car pos) (cdr pos))))
        (loop for npos in (2d-neighbors grid pos)
              when (= (1+ x) (aref grid (car npos) (cdr npos)))
                collect (cons pos (hike grid npos))))))

(defun day-10-part-1 (input-file) (progn input-file -1))

(defun day-10-part-2 (input-file) (progn input-file -1))

(defun day-10 ()
  (let ((f (fetch-day-input-file 2024 10)))
    (values (day-10-part-1 f)
            (day-10-part-2 f))))
