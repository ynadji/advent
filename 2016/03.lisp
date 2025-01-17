(in-package :aoc2016)

(defun triangle? (sides)
  (destructuring-bind (a b c) sides
    (and (> (+ a b) c)
         (> (+ a c) b)
         (> (+ b c) a))))

(defun day-03-part-1 (input-file)
  (length (remove-if-not #'triangle? (mapcar #'string-to-num-list (uiop:read-file-lines input-file)))))

(defun day-03-part-2 (input-file)
  (labels ((read-triangle-columns (input-file)
             (let ((lines (mapcar #'string-to-num-list (uiop:read-file-lines input-file))))
               (partition (ax:flatten (transpose lines)) 3))))
    (length (remove-if-not #'triangle? (read-triangle-columns input-file)))))

(defun day-03 ()
  (let ((f (fetch-day-input-file 2016 3)))
    (values (day-03-part-1 f)
            (day-03-part-2 f))))
