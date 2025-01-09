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
    (values min-state min-score)))

;; TODO: subclass cl-heap:fibonacci-heap to also track HEAP-MAP internally so
;; you can just do decrease-key directly with whatever you're storing.
(defun make-heap (dist)
  (flet ((my-key (obj &rest values)
           (if values
               (setf (gethash obj dist) (car values))
               (gethash obj dist))))
    (let ((heap (make-instance 'cl-heap:fibonacci-heap :key #'my-key))
          (heap-map (make-hash-table :test #'equal :size (hash-table-size dist))))
      (loop for state being the hash-key of dist
            do (setf (gethash state heap-map)
                     (nth-value 1 (cl-heap:add-to-heap heap state))))
      (values heap heap-map))))

(defun 16-reachable? (m pos dir)
  (declare (ignorable dir))
  (char/= #\# (paref m pos)))

(defun dijkstra (start maze)
  (let* ((dist (initialize-dist maze))
         (prev (make-hash-table :test #'equal))
         heap)
    (setf (gethash start dist) 0)
    (multiple-value-bind (heap heap-map) (make-heap dist)
      (loop for state = (cl-heap:pop-heap heap)
            while state
            for dir = (car state)
            for (new-states new-dirs) = (multiple-value-list (2d-neighbors maze (cdr state)
                                                                           :reachable? #'16-reachable?))
            do (loop for new-state in (mapcar #'cons new-dirs new-states)
                     for new-dir = (car new-state)
                     for new-cost = (cond ((eq new-dir dir) 1)
                                          ((eq (opposite-direction dir) new-dir) 2001)
                                          (t 1001))
                     do (let ((alt (+ (gethash state dist) new-cost)))
                          (when (< alt (gethash new-state dist))
                            (cl-heap:decrease-key heap (gethash new-state heap-map) alt)
                            (setf (gethash new-state dist) alt)
                            (setf (gethash new-state prev) (list state)))
                          (when (= alt (gethash new-state dist))
                            (pushnew state (gethash new-state prev)))))))
    (values dist prev heap)))

(defun day-16-part-1 (input-file)
  (multiple-value-bind (maze starts) (read-grid input-file :starts? (lambda (c) (member c '(#\S #\E))))
    (let ((start (cons :east (first starts))))
      (multiple-value-bind (dist prev) (dijkstra start maze)
        (multiple-value-bind (min-state min-score)
            (min-score-state (loop for dir in *cardinals* collect (cons dir (second starts))) dist)
          (values min-score prev min-state start))))))

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
          (multiple-value-bind (dist prev) (dijkstra start maze)
            (multiple-value-bind (min-state min-score)
                (min-score-state (loop for dir in *cardinals* collect (cons dir (second starts))) dist)
              (declare (ignore min-score))
              (length (remove-duplicates (mapcar #'cdr (walk-back prev min-state start)) :test #'equal))))))))

(defun day-16 ()
  (let ((f (fetch-day-input-file 2024 16)))
    (multiple-value-bind (min-score prev min-state start) (day-16-part-1 f)
     (values min-score
             (day-16-part-2 f prev min-state start)))))
