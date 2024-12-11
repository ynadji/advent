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

(defun hike% (grid pos)
  (if (= 9 (aref grid (car pos) (cdr pos)))
      (list pos)
      (let ((x (aref grid (car pos) (cdr pos))))
        (loop for npos in (2d-neighbors grid pos)
              when (= (1+ x) (aref grid (car npos) (cdr npos)))
                nconc (hike% grid npos)))))

(defun hike (grid trailheads part)
  (loop for trailhead in trailheads
        nconc (if (= 1 part)
                  (remove-duplicates (hike% grid trailhead) :test #'equal)
                  (hike% grid trailhead))))

(defun day-10 ()
  (let ((f (fetch-day-input-file 2024 10)))
    (multiple-value-bind (grid trailheads) (read-grid f :element-type 'fixnum :starts? (lambda (x) (= x 0)))
      (values (length (hike grid trailheads 1))
              (length (hike grid trailheads 2))))))
