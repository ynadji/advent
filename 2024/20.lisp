(in-package :aoc2024)

(defparameter test-input "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(declaim (optimize (speed 3)))

(defun add-all-dirs (pos)
  (loop for dir in *cardinals* collect (cons dir pos)))

(defun get-optimal-path (input-file)
  (multiple-value-bind (maze starts-and-ends) (read-grid input-file :starts? (lambda (c) (member c '(#\S #\E))))
    (multiple-value-bind (starts ends)
        (if (char= #\S (paref maze (first starts-and-ends)))
            (values (add-all-dirs (first starts-and-ends)) (add-all-dirs (second starts-and-ends)))
            (values (add-all-dirs (second starts-and-ends)) (add-all-dirs (first starts-and-ends))))
      (multiple-value-bind (dist prev) (aoc-utils:dijkstra starts maze :reachable? #'16-reachable?)
        (multiple-value-bind (min-state min-score)
            (aoc-utils:min-score-state ends dist)
          (declare (ignore min-score))
          (cons (cdr (first starts)) (reverse (remove-duplicates (mapcar #'cdr (walk-back prev min-state (cdar starts))) :test #'equal))))))))

;; this can probably be further improved but it's < 1 sec so let's
;; move on.
(defun day-20% (path &optional (d 2))
  (declare (type fixnum d))
  (the fixnum (loop for i fixnum from 0 for x in path
        sum
        (the fixnum (loop for j fixnum from 0 for y in path
              when (and (< i j) (<= (the fixnum (manhattan-distance x y)) d))
              count (>= (- (the fixnum (abs (the fixnum (- i j)))) (the
 fixnum (manhattan-distance x y))) 100))))))

(defun day-20 ()
  (let* ((f (fetch-day-input-file 2024 20))
         (path (get-optimal-path f)))
    (values (day-20% path 2)
            (day-20% path 20))))
