(in-package :aoc2015)

(defun surface-area (l w h)
  (+ (* 2 l w) (* 2 w h) (* 2 h l)
     (destructuring-bind (x y z) (sort (list l w h) #'<)
       (declare (ignore z))
       (* x y))))

(defun ribbon-length (l w h)
  (+ (* l w h)
     (destructuring-bind (x y z) (sort (list l w h) #'<)
       (declare (ignore z))
       (+ (* 2 x) (* 2 y)))))

(defun day-02-part-1 (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        for (l w h) = (mapcar #'parse-integer (str:split "x" line))
        sum (surface-area l w h)))

(defun day-02-part-2 (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        for (l w h) = (mapcar #'parse-integer (str:split "x" line))
        sum (ribbon-length l w h)))

(defun day-02 ()
  (let ((f (fetch-day-input-file 2015 2)))
    (values (day-02-part-1 f)
            (day-02-part-2 f))))
