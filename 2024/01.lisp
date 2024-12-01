(in-package :aoc2024)

(defun read-location-ids (input-file)
  (unzip
   (mapcar (lambda (line) (mapcar #'parse-integer
                             (cl-ppcre:split "\\s+" line)))
           (uiop:read-file-lines input-file))))

(defun day-01-part-1 (input-file)
  (let ((lists (read-location-ids input-file)))
    (loop for x in (sort (first lists) #'<)
          for y in (sort (second lists) #'<)
          sum (abs (- x y)))))

(defun day-01-part-2 (input-file)
  (let* ((lists (read-location-ids input-file))
         (freqs (serapeum:frequencies (second lists))))
    (loop for x in (first lists)
          sum (* x (gethash x freqs 0)))))

(defun day-01 ()
  (let ((f (fetch-day-input-file 2024 1)))
    (values (day-01-part-1 f)
            (day-01-part-2 f))))
