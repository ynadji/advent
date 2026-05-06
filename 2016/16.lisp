(in-package :aoc2016)

(defun fill-disk-step (a)
  (reverse (map 'string (lambda (c) (if (char= c #\0) #\1 #\0)) a)))

(defun fill-disk-until (a limit)
  (if (>= (length a) limit)
      (subseq a 0 limit)
      (fill-disk-until (concatenate 'string a "0" (fill-disk-step a)) limit)))

(defun disk-checksum (a)
  (flet ((checksum% (a)
           (coerce (loop for (x y) on (coerce a 'list) by #'cddr
                         collect (if (char= x y) #\1 #\0))
                   'string)))
    (let ((chksum (checksum% a)))
      (if (oddp (length chksum))
          chksum
          (disk-checksum chksum)))))

(defun day-16% (input-file)
  (let ((a (str:trim (uiop:read-file-string input-file))))
    (values (disk-checksum (fill-disk-until a 272))
            (disk-checksum (fill-disk-until a 35651584)))))

(defun day-16 ()
  (let ((f (fetch-day-input-file 2016 16)))
    (day-16% f)))
