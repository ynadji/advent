(in-package :aoc2024)

(defparameter test-input-me "#####
#..E#
#.#.#
#S..#
####")

(defparameter test-input-1 "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(defparameter test-input-2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

(defun 16-reachable? (m pos dir)
  (declare (ignorable dir))
  (char/= #\# (paref m pos)))

(defun 16-cost-fn (s0 s1)
  (let ((dir (car s0))
        (new-dir (car s1)))
    (cond ((eq new-dir dir) 1)
          ((eq (opposite-direction dir) new-dir) 2001)
          (t 1001))))

(defun day-16-part-1 (input-file)
  (multiple-value-bind (maze starts) (read-grid input-file :starts? (lambda (c) (member c '(#\S #\E))))
    (let ((start (cons :east (first starts))))
      (multiple-value-bind (dist prev) (aoc-utils:dijkstra (list start) maze :cost-fn #'16-cost-fn :reachable? #'16-reachable?)
        (multiple-value-bind (min-state min-score)
            (aoc-utils:min-score-state (loop for dir in *cardinals* collect (cons dir (second starts))) dist)
          (values min-score prev min-state start))))))

;; TODO: Maybe worth adding to utils?
(defun walk-back (prev state start-state)
  (if (equal state start-state)
      (cons start-state nil)
      (let ((next-states (gethash state prev)))
        (loop for next-state in next-states append (cons state (walk-back prev next-state start-state))))))

(defun day-16-part-2 (input-file &optional prev min-state start)
  (if (and prev min-state start)
      (length (remove-duplicates (mapcar #'cdr (walk-back prev min-state start)) :test #'equal))
      (multiple-value-bind (maze starts) (read-grid input-file :starts? (lambda (c) (member c '(#\S #\E))))
        (let ((start (cons :east (first starts))))
          (multiple-value-bind (dist prev) (aoc-utils:dijkstra (list start) maze :cost-fn #'16-cost-fn :reachable? #'16-reachable?)
            (multiple-value-bind (min-state min-score)
                (aoc-utils:min-score-state (loop for dir in *cardinals* collect (cons dir (second starts))) dist)
              (declare (ignore min-score))
              (length (remove-duplicates (mapcar #'cdr (walk-back prev min-state start)) :test #'equal))))))))

(defun day-16 ()
  (let ((f (fetch-day-input-file 2024 16)))
    (multiple-value-bind (min-score prev min-state start) (day-16-part-1 f)
     (values min-score
             (day-16-part-2 f prev min-state start)))))
