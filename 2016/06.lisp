(in-package :aoc2016)

(defun day-06% (input-file sort-fun)
  (flet ((most-frequent-char (column)
           (caar (sort (frequencies column) sort-fun :key #'cdr))))
   (let ((lines (uiop:read-file-lines input-file)))
     (coerce (mapcar #'most-frequent-char (transpose (mapcar (lambda (s) (coerce s 'list))
                                                             lines))) 'string))))

(defun day-06 ()
  (let ((f (fetch-day-input-file 2016 6)))
    (values (day-06% f #'>)
            (day-06% f #'<))))
