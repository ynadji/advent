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

(defstruct (path (:print-function print-path))
  state (previous nil) direction (cost-so-far 0) (total-cost 0))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (path-state path) (path-total-cost path)))

(defun solve-maze (paths maze goal? cost-fn cost-left-fn &optional (state= #'equal) old-paths)
  (cond ((null paths) (error "ruh roh"))
        ((funcall goal? (path-state (first paths)))
         (values (first paths) paths))
        (t (let* ((path (pop paths))
                  (state (path-state path))
                  (direction (path-direction path)))
             (setf old-paths (insert-path path old-paths))
             (multiple-value-bind (new-states new-dirs)
                 (2d-neighbors maze state :reachable? (lambda (m pos dir) (declare (ignorable dir))
                                                                (char/= #\# (paref m pos))))
               ;; TODO make sure we aren't retracting steps (e.g., (cons direction pos) is new)
               (loop for new-state in new-states for new-dir in new-dirs
                     for cost = (+ (path-cost-so-far path)
                                   (cond ((eq new-dir direction) 1)
                                         ((eq (opposite-direction direction) new-dir) 2001)
                                         (t 1001)))
                     for path2 = (make-path :state new-state :direction new-dir :previous path :cost-so-far cost)
                     do (let ((old nil))
                          (cond
                            ((setf old (find-path new-state paths state=))
                             (when (better-path path2 old)
                               (setf paths (insert-path
                                            path2 (delete old paths)))))
                            ((setf old (find-path new-state old-paths state=))
                             (when (better-path path2 old)
                               (setf paths (insert-path path2 paths))
                               (setf old-paths (delete old old-paths))))
                            (t (setf paths (insert-path path2 paths))))))
               (solve-maze paths maze goal? cost-fn cost-left-fn state= old-paths))))))


(defun find-path (state paths state=)
  "Find the path with this state among a list of paths."
  (find state paths :key #'path-state :test state=))

(defun better-path (path1 path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insert-path (path paths)
  "Put path into the right position, sorted by total cost."
  ;; MERGE is a built-in function
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this path."
  (if (null path)
      nil
      (cons (path-state path)
            (path-states (path-previous path)))))

(defun day-16-part-1 (input-file) (multiple-value-bind (maze starts) (read-grid input-file :starts? (lambda (c) (member c '(#\S #\E))))
    (destructuring-bind (start goal) starts
      (let ((path (make-path :state start :direction :east)))
        (solve-maze (list path) maze (lambda (s) (equal s goal)) nil nil)))))

(defun day-16-part-2 (input-file)
  (progn input-file -1))

(defun day-16 ()
  (let ((f (fetch-day-input-file 2024 16)))
    (values (day-16-part-1 f)
            (day-16-part-2 f))))
