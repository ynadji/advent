(in-package :aoc2024)

(defparameter test-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defparameter other-test-input "..........
..........
..........
....a.....
..........
.....a....
..........
..........
..........
..........")

(defparameter test-input-3 "T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
..........")

(defun read-antenna-map (input-file)
  (let ((lines (uiop:read-file-lines input-file))
        (map (make-hash-table)))
    (loop for i from 0 for line in lines do
      (loop for j from 0 for c across line
            unless (eq c #\.)
              do (push (list i j) (gethash c map))))
    (values map (length lines) (length (first lines)))))

(defun find-antinodes% (p1 p2 nrows ncols &optional (part 1))
  (labels ((in-bounds? (p)
             (and (< -1 (first p) nrows)
                  (< -1 (second p) ncols)))
           (original? (p)
             (or (equal p p1)
                 (equal p p2))))
    (let ((offset (mapcar #'- p1 p2)))
      (if (= 1 part)
          (remove-if #'original?
                     (remove-if-not #'in-bounds?
                                    (list (mapcar #'+ p1 offset)
                                          (mapcar #'- p1 offset)
                                          (mapcar #'+ p2 offset)
                                          (mapcar #'- p2 offset))))
          (loop for x from 1
                for next-offset = (mapcar (lambda (o) (* o x)) offset)
                for positions = (remove-if-not #'in-bounds?
                                               (list (mapcar #'+ p1 next-offset)
                                                     (mapcar #'- p1 next-offset)
                                                     (mapcar #'+ p2 next-offset)
                                                     (mapcar #'- p2 next-offset)))
                while positions
                append positions)))))

(defun find-antinodes (map nrows ncols &optional (part 1))
  (length
   (remove-duplicates
    (loop for positions in (ax:hash-table-values map)
          append
          (loop for pos1 in positions
                append
                (loop for pos2 in positions
                      append
                      (unless (equal pos1 pos2)
                        (find-antinodes% pos1 pos2 nrows ncols part)))))
    :test #'equal)))

(defun day-08-part-1 (input-file)
  (multiple-value-bind (m nrows ncols) (read-antenna-map input-file)
   (find-antinodes m nrows ncols)))

(defun day-08-part-2 (input-file)
  (multiple-value-bind (m nrows ncols) (read-antenna-map input-file)
   (find-antinodes m nrows ncols 2)))

(defun day-08 ()
  (let ((f (fetch-day-input-file 2024 8)))
    (values (day-08-part-1 f)
            (day-08-part-2 f))))
