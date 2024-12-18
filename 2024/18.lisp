(in-package :aoc2024)

(defparameter test-input "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(defun dijkstra2 (starts maze)
  (let* ((dist (initialize-dist maze))
         (prev (make-hash-table :test #'equal)))
    (loop for start in starts do (setf (gethash start dist) 0))
    (multiple-value-bind (heap heap-map) (make-heap dist)
      (loop for state = (cl-heap:pop-heap heap)
            while state
            for dir = (car state)
            for (new-states new-dirs) = (multiple-value-list (2d-neighbors maze (cdr state)
                                                                           :reachable? #'16-reachable?))
            do (loop for new-state in (mapcar #'cons new-dirs new-states)
                     for new-dir = (car new-state)
                     for new-cost = 1
                     do ;;(format t "~a -> ~a~%" state new-state)
                        (let ((alt (+ (gethash state dist) new-cost)))
                          (when (< alt (gethash new-state dist))
                            (cl-heap:decrease-key heap (gethash new-state heap-map) alt)
                            (setf (gethash new-state dist) alt)
                            (setf (gethash new-state prev) (list state)))
                          (when (= alt (gethash new-state dist))
                            (pushnew state (gethash new-state prev))))))
      (values dist prev heap))))

(defun read-bytes (input-file &optional (nbytes 12) (dimensions '(7 7)))
  (let ((memory (make-array dimensions :element-type 'standard-char :initial-element #\.)))
    (loop repeat nbytes
          for (j i) in (mapcar #'string-to-num-list (uiop:read-file-lines input-file))
          do (setf (aref memory i j) #\#))
    memory))

(defun day-18-part-1 (input-file &optional (nbytes 1024))
  (let ((memory (read-bytes input-file nbytes '(71 71))))
    (multiple-value-bind (dist prev heap) (dijkstra2 (list (cons :east (cons 0 0))
                                                           (cons :south (cons 0 0))) memory)
      (declare (ignore heap))
      (multiple-value-bind (min-state min-score)
          (min-score-state (loop for dir in *cardinals* collect (cons dir (cons 70 70))) dist)
        (values min-score min-state)))))

;; so binary search would probably be faster?
(defun day-18-part-2 (input-file)
  (loop for nbytes from 2898
        when (= most-positive-fixnum (day-18-part-1 input-file nbytes))
          return (nth (1- nbytes) (uiop:read-file-lines input-file))))

(defun day-18 ()
  (let ((f (fetch-day-input-file 2024 18)))
    (values (day-18-part-1 f)
            (day-18-part-2 f))))
