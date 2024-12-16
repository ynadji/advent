(in-package :aoc2024)

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

(declaim (optimize (debug 3)))

(defstruct (path (:print-function print-path))
  state (previous nil) direction (cost-so-far 0) (total-cost 0))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (path-state path) (path-total-cost path)))

(defun initialize-dist (maze)
  (let ((dist (make-hash-table :test #'equal :size (* 4 (array-total-size maze)))))
    (loop for i below (array-dimension maze 0) do
      (loop for j below (array-dimension maze 1) do
        (loop for dir in *cardinals* do
          (unless (char= #\# (aref maze i j))
           (setf (gethash (cons dir (cons i j)) dist) most-positive-fixnum)))))
    dist))

(defun min-score-state (states dist)
  (let ((min-state (first states))
        (min-score most-positive-fixnum))
    (loop for state in states
          for state-score = (gethash state dist)
          when (< state-score min-score)
            do (setf min-state state min-score state-score))
    ;;(format t "min-state: ~a~%" min-state)
    (values min-state min-score)))

(defun dijkstra (start maze)
  (let* ((dist (initialize-dist maze))
         (prev (make-hash-table :test #'equal))
         (states (ax:hash-table-keys dist)))
    (setf (gethash start dist) 0)
    (loop while states
          for state = (min-score-state states dist)
          for dir = (car state)
          for (new-states new-dirs) = (multiple-value-list (2d-neighbors maze (cdr state)
                                                                         :reachable? (lambda (m pos dir)
                                                                                       (declare (ignorable dir))
                                                                                       (char/= #\# (paref m pos)))))
          do ;;(format t "state: ~a, dir: ~a~%" state dir)
             ;;(format t "new-states: ~a, new-dirs: ~a~%" new-states new-dirs)
             ;;(format t "new-new-states: ~a~%" (mapcar #'cons new-dirs new-states))
             ;;(format t "new-new-states-have: ~a~%" (intersection states (mapcar #'cons new-dirs new-states) :test #'equal))
             (loop for new-state in (intersection states (mapcar #'cons new-dirs new-states) :test #'equal)
                   for new-dir = (car new-state)
                   for new-cost = (cond ((eq new-dir dir) 1)
                                        ((eq (opposite-direction dir) new-dir) 2001)
                                        (t 1001))
                   do ;;(format t "    new-state: ~a, new-dir: ~a, cost: ~a~%" new-state new-dir new-cost)
                      (let ((alt (+ (gethash state dist) new-cost)))
                        (when (< alt (gethash new-state dist))
                          (setf (gethash new-state dist) alt)
                          (setf (gethash new-state prev) (list state)))
                        (when (= alt (gethash new-state dist))
                          (pushnew state (gethash new-state prev)))))
             (setf states (remove state states :test #'equal)))
    (values dist prev)))

(defun day-16-part-1 (input-file)
  (multiple-value-bind (maze starts) (read-grid input-file :starts? (lambda (c) (member c '(#\S #\E))))
    (let ((start (cons :east (first starts))))
      (multiple-value-bind (dist prev) (dijkstra start maze)
        (nth-value 1 (min-score-state (loop for dir in *cardinals* collect (cons dir (second starts))) dist))))))

(defun walk-back (prev state start-state)
  (if (equal state start-state)
      (cons start-state nil)
      (let ((next-states (gethash state prev)))
        (loop for next-state in next-states append (cons state (walk-back prev next-state start-state))))))

(defun day-16-part-2 (input-file)
  (multiple-value-bind (maze starts) (read-grid input-file :starts? (lambda (c) (member c '(#\S #\E))))
    (let ((start (cons :east (first starts))))
      (multiple-value-bind (dist prev) (dijkstra start maze)
        (multiple-value-bind (min-state min-score)
            (min-score-state (loop for dir in *cardinals* collect (cons dir (second starts))) dist)
          (declare (ignorable min-score))
          (length (remove-duplicates (mapcar #'cdr (walk-back prev min-state start)) :test #'equal)))))))

(defun day-16 ()
  (let ((f (fetch-day-input-file 2024 16)))
    (values (day-16-part-1 f)
            (day-16-part-2 f))))
