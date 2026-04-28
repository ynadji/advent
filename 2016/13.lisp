(in-package :aoc2016)

(defun wall? (x y fav-num)
  (let ((num (+ fav-num (* x x) (* 3 x) (* 2 x y) y (* y y))))
    (oddp (count #\1 (format nil "~b" num)))))

(defun make-grid (fave-num)
  (let ((grid (make-array (list fave-num fave-num) :element-type 'standard-char)))
    (aops:each-index (i j)
      (setf (aref grid i j) (if (wall? j i fave-num) #\# #\.)))
    grid))

;; (39 . 31)
(defun solve-maze (grid &optional (start '(1 . 1)) (end '(4 . 7)))
  (flet ((successors (pos)
           (2d-neighbors grid pos :reachable? (lambda (m pos dir)
                                                (declare (ignore dir))
                                                (char= #\. (paref m pos)))))
         (finished? (pos) (equal pos end)))
    (path-cost-so-far (a*-search (list (make-path :state start)) #'finished? #'successors (lambda (x y) (declare (ignore x y)) 1) (lambda (x) (declare (ignore x)) 1) #'equal))))

(defun day-13-part-1 (input-file)
  (solve-maze (make-grid (parse-integer (uiop:read-file-string input-file)))
              '(1 . 1) '(39 . 31)))

(defun day-13-part-2 (input-file) (progn input-file -1))

(defun day-13 ()
  (let ((f (fetch-day-input-file 2016 13)))
    (values (day-13-part-1 f)
            (day-13-part-2 f))))
