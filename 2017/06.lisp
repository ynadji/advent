(in-package :aoc2017)

(defparameter test-input "0    2   7    0")

(defun reallocate (list)
  (multiple-value-bind (idx max) (argmax list)
    (let ((list (copy-list list))
          (length (length list)))
      (setf (nth idx list) 0)
      (loop while (plusp max)
            with i = (mod (1+ idx) length)
            do (incf (nth i list))
               (decf max)
               (setf i (mod (1+ i) length)))
      list)))

(defun reallocate-cycle (list &optional (seen-times 1))
  (let ((seen (make-hash-table :test #'equal)))
    (loop for i from 0
          until (= (gethash list seen 0) seen-times)
          do (incf (gethash list seen 0))
             (setf list (reallocate list))
          finally (return i))))

(defun day-06% (input-file)
  (let* ((list (string-to-num-list (uiop:read-file-string input-file)))
         (first-cycle-count (reallocate-cycle list)))
    (values first-cycle-count
            (- (reallocate-cycle list 2) first-cycle-count))))

(defun day-06 ()
  (let ((f (fetch-day-input-file 2017 6)))
    (day-06% f)))
