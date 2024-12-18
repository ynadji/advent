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

(defun reset-memory (memory bytes nbytes &optional fresh?)
  (unless fresh?
    (loop for i below (array-total-size memory)
          do (setf (row-major-aref memory i) #\.)))
  (loop for n from 0 for (j i) across bytes
        when (<= n nbytes)
          do (setf (aref memory i j) #\#)))

(defun read-bytes (input-file &optional (nbytes 12) (dimensions '(7 7)))
  (let* ((memory (make-array dimensions :element-type 'standard-char :initial-element #\.))
         (bytes* (mapcar #'string-to-num-list (uiop:read-file-lines input-file)))
         (bytes (make-array (length bytes*) :initial-contents bytes*)))
    (reset-memory memory bytes nbytes t)
    (values memory bytes)))

(defun find-shortest-path-score (memory start end)
  (multiple-value-bind (dist prev heap) (dijkstra2 (list (cons :east start)
                                                         (cons :south start)) memory)
    (declare (ignore heap))
    (multiple-value-bind (min-state min-score)
        (min-score-state (loop for dir in *cardinals* collect (cons dir end)) dist)
      (values min-score min-state))))

(defun day-18-part-1 (input-file &optional (nbytes 1024))
  (let ((memory (read-bytes input-file nbytes '(71 71))))
    (find-shortest-path-score memory (cons 0 0) (cons 70 70))))

(defun undo-byte (memory ij)
  (destructuring-bind (j i) ij
    (setf (aref memory i j) #\.)))

(defun binary-search (memory bytes left right &optional (start (cons 0 0)) (end (cons 70 70)))
  (flet ((no-path? (memory bytes nbytes)
           (reset-memory memory bytes nbytes)
           (= most-positive-fixnum (find-shortest-path-score memory start end)))
         (middle (l r)
           (+ l (truncate (/ (- r l) 2)))))
    (loop for mid = (middle left right)
          ;;do (format t "trying ~a (~a, ~a)~%" mid left right)
          if (no-path? memory bytes mid)
            do ;(format t "    ~a failed, setting right to ~a~%" mid mid)
               (setf right mid)
          else
            do ;(format t "    ~a succeeded, setting left to ~a~%" mid mid)
               (setf left mid)
          when (= (1+ left) right)
            do (return mid))))

;; binary search: 439ms
;; reverse: ~14s
;; parallel:
(defun day-18-part-2 (input-file)
  (multiple-value-bind (memory bytes) (read-bytes input-file 1024 '(71 71))
    (let ((first-good (binary-search memory bytes 1025 3450)))
      (format nil "~{~a~^,~}" (aref bytes first-good)))))


(defun day-18 ()
  (let ((f (fetch-day-input-file 2024 18)))
    (values (day-18-part-1 f)
            (day-18-part-2 f))))
