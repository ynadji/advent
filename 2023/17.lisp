(in-package :aoc2023)

(defparameter test-input-1 "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(defclass stepper (state)
  ((steps :accessor steps :initarg :steps :initform 0)))

(defmethod print-object ((state stepper) out)
  (print-unreadable-object (state out :type t)
    (format out "~a ~a heading ~a" (steps state) (pos state) (dir state))))

(defun filter-steps (states prev-direction)
  (remove-if (lambda (s) (or (> (steps s) 3)
                        (eq prev-direction (opposite-direction (dir s)))))
             states))

(defmethod aoc-utils::neighbor-states% (grid (stepper stepper) reachable? wanted-directions)
  (let ((next-states (mapcar (lambda (c) (change-class c 'stepper)) (call-next-method)))
        (last-dir (dir stepper))
        (last-steps (steps stepper)))
    (loop for state in next-states
          if (eq last-dir (dir state))
            do (incf (steps state) (if (plusp last-steps) (1+ last-steps) 1))
          else
            do (setf (steps state) 1))
    (filter-steps next-states last-dir)))

(defun day-17% (maze)
  (flet ((cost-fn (s0 s1)
           (declare (ignore s0))
           (paref maze (pos s1))))
    (let ((starts (list (make-instance 'stepper :dir :none :pos '(0 . 0)))))
      (let ((dist (dijkstra starts maze :cost-fn #'cost-fn))
            (bottom-right (destructuring-bind (i j)
                              (mapcar #'1- (array-dimensions maze))
                            (cons i j))))
        (loop for dir in *cardinals* minimize
              (gethash (make-instance 'stepper :dir dir :pos bottom-right) dist))))))

(defun day-17-part-1 (input-file) (progn input-file -1))

(defun day-17-part-2 (input-file) (progn input-file -1))

(defun day-17 ()
  (let ((f (fetch-day-input-file 2023 17)))
    (values (day-17-part-1 f)
            (day-17-part-2 f))))
