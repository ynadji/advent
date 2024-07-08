(in-package :aoc2023)

;; TODO: gotta be a more elegant way to do this.
(defun count-sharpsign-runs (spring)
  (let ((counts)
        (count 0))
    (declare (fixnum count))
    (loop for c in spring
          do (if (char= c #\#)
                 (incf count)
                 (when (> count 0)
                   (push count counts)
                   (setf count 0)))
          finally (when (> count 0)
                    (push count counts)
                    (setf count 0)))
    counts))

(defun possible-arrangement? (spring-condition nums)
  (equal nums
         (count-sharpsign-runs spring-condition)))

(defun find-all-possible-arrangements (spring-condition nums &optional arrangement)
  (if (and (null spring-condition)
           (possible-arrangement? arrangement nums))
      (list arrangement)
      (let ((c (first spring-condition)))
        (append
         (case c
           ((#\# #\.) (find-all-possible-arrangements (rest spring-condition) nums (cons c arrangement)))
           (#\? (append
                 (find-all-possible-arrangements (rest spring-condition) nums (cons #\. arrangement))
                 (find-all-possible-arrangements (rest spring-condition) nums (cons #\# arrangement)))))))))

(defun read-springs (input-file)
  (let ((tmp (mapcar (lambda (x) (str:split " " x))
                     (uiop:read-file-lines input-file))))
    (values (->> tmp
              (mapcar #'first)
              (mapcar (lambda (s) (coerce s 'list))))
            (->> tmp
              (mapcar #'second)
              (mapcar (lambda (x) (str:split "," x)))
              (mapcar (lambda (x) (mapcar #'parse-integer x)))))))

(defun day-12-part-1 (input-file)
  (multiple-value-bind (springs list-of-nums) (read-springs input-file)
    (loop for spring in springs for nums in list-of-nums
          sum (length (find-all-possible-arrangements spring nums)))))

(defun get-all-valid-springs (input-file)
  (multiple-value-bind (springs list-of-nums) (read-springs input-file)
    (loop for spring in springs for nums in list-of-nums
          collect (find-all-possible-arrangements spring nums))))
