(in-package :aoc2016)

(defun wall? (x y fav-num)
  (let ((num (+ fav-num (* x x) (* 3 x) (* 2 x y) y (* y y))))
    (oddp (count #\1 (format nil "~b" num)))))

(defun make-grid (fave-num)
  (let ((grid (make-array (list fave-num fave-num) :element-type 'standard-char)))
    (aops:each-index (i j)
      (setf (aref grid i j) (if (wall? j i fave-num) #\# #\.)))
    grid))

(defun day-13-part-1 (input-file) (progn input-file -1))

(defun day-13-part-2 (input-file) (progn input-file -1))

(defun day-13 ()
  (let ((f (fetch-day-input-file 2016 13)))
    (values (day-13-part-1 f)
            (day-13-part-2 f))))
