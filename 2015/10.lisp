(in-package :aoc2015)

(defun look-and-say% (chars)
  (loop for part in (partition-by chars)
        collect (digit-char (length part))
        collect (first part)))

(defun look-and-say (s n)
  (loop with chars = (coerce s 'list) repeat n
        do (setf chars (look-and-say% chars))
        finally (return (length chars))))

(defun day-10 ()
  (let ((s (str:trim (uiop:read-file-string (fetch-day-input-file 2015 10)))))
    (values (look-and-say s 40)
            (look-and-say s 50))))
