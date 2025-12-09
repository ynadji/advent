(in-package :aoc2025)

(defparameter test-input "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defun read-red-squares (input-file)
  (coerce (loop for line in (uiop:read-file-lines input-file)
                collect (string-to-num-list line))
          'vector))

(defun rectangle-area (p1 p2)
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (* (1+ (abs (- x2 x1)))
         (1+ (abs (- y2 y1)))))))

(defun day-09-part-1 (input-file)
  (let ((points (read-red-squares input-file)))
    (loop for i from 0 below (array-dimension points 0)
          maximize (loop for j from (1+ i) below (array-dimension points 0)
                         maximize (rectangle-area (aref points i) (aref points j))))))

(defun day-09-part-2 (input-file) (progn input-file -1))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2025 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
