(in-package :aoc2015)

(defun prefix-five-0s? (arr)
  (and (zerop (aref arr 0))
       (zerop (aref arr 1))
       (< (aref arr 2) #x10)))

(defun prefix-six-0s? (arr)
  (and (zerop (aref arr 0))
       (zerop (aref arr 1))
       (zerop (aref arr 2))))

(defun five-0-prefix-hash (key)
  (loop for i from 1
        when (prefix-five-0s? (md5:md5sum-string (format nil "~a~a" key i)))
        do (return i)))

(defun six-0-prefix-hash (key)
  (loop for i from 1
        when (prefix-six-0s? (md5:md5sum-string (format nil "~a~a" key i)))
        do (return i)))

(defun day-04-part-1 (input-file)
  (five-0-prefix-hash (str:trim (uiop:read-file-string input-file))))

(defun day-04-part-2 (input-file)
  (six-0-prefix-hash (str:trim (uiop:read-file-string input-file))))

(defun day-04 ()
  (let ((f (fetch-day-input-file 2015 4)))
    (values (day-04-part-1 f)
            (day-04-part-2 f))))
