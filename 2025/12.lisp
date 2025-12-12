(in-package :aoc2025)

(defparameter test-input "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")

(defun parse-regions (input-file)
  (let ((fields (str:split (format nil "~c~c" #\Newline #\Newline) (str:trim (uiop:read-file-string input-file)))))
    (mapcar #'string-to-num-list (str:split #\Newline (ax:lastcar fields)))))

(defun day-12-part-1 (input-file)
  (let ((n 0))
    (dolist (region (parse-regions input-file) n)
      (destructuring-bind (w h &rest nums) region
        (when (>= (* (floor w 3) (floor h 3)) (reduce #'+ nums))
          (incf n))))))

(defun day-12-part-2 (input-file) (progn input-file 0))

(defun day-12 ()
  (let ((f (fetch-day-input-file 2025 12)))
    (values (day-12-part-1 f)
            (day-12-part-2 f))))
