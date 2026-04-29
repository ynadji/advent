(in-package :aoc2016)

(defun wall? (x y fav-num)
  (declare (optimize speed)
           (type fixnum x y fav-num))
  (let ((num (the fixnum (+ fav-num (* x x) (* 3 x) (* 2 x y) y (* y y)))))
    (oddp (logcount num))))

;; TODO: Most of the time is from making the grid. If you just did math for the successors it would probably be instant.
(defun make-grid (fave-num)
  (let ((grid (make-array (list fave-num fave-num) :element-type 'standard-char)))
    (aops:each-index (i j)
      (setf (aref grid i j) (if (wall? j i fave-num) #\# #\.)))
    grid))

(defun solve-maze (grid &optional (start '(1 . 1)) (end '(4 . 7)))
  (flet ((successors (pos)
           (2d-neighbors grid pos :reachable? (lambda (m pos dir)
                                                (declare (ignore dir))
                                                (char= #\. (paref m pos))))))
    (multiple-value-bind (path old-paths)
        (graph-search (list (make-path :state start)) (equals end :key #'path-state :test #'equal) (path-saver #'successors (constantly 1) (constantly 1)) #'prepend (lambda (x y) (equal (path-state x) (path-state y))))
      (values (path-cost-so-far path)
              (loop for path in old-paths
                    count (<= (path-cost-so-far path) 50))))))

(defun day-13% (input-file)
  (solve-maze (make-grid (parse-integer (uiop:read-file-string input-file)))
              '(1 . 1) '(39 . 31)))

(defun day-13 ()
  (let ((f (fetch-day-input-file 2016 13)))
    (day-13% f)))
