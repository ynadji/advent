(in-package :aoc2016)

(defparameter test-input "ADVENT
A(1x5)BC
(3x3)XYZ
A(2x2)BCD(2x2)EFG
(6x1)(1x3)A
X(8x2)(3x3)ABCY")

(defun decompressed-length (s &key (start 0) (end (length s)) part2?)
  (loop with i = start with total = 0 while (< i end) do
    (let ((c (char s i)))
      (if (char= c #\()
          (multiple-value-bind (ms me rs re) (cl-ppcre:scan "\\((\\d+)x(\\d+)\\)" s :start i)
            (declare (ignore ms))
            (let ((n (parse-integer s :start (aref rs 0) :end (aref re 0)))
                  (m (parse-integer s :start (aref rs 1) :end (aref re 1))))
              (incf total (* m (if part2?
                                   (decompressed-length s :start me :end (+ me n) :part2? t)
                                   n)))
              (setf i (+ me n))))
          (progn (incf total) (incf i))))
        finally (return total)))

(defun day-09-part-1 (input-file)
  (let ((s (uiop:read-file-string input-file)))
    (decompressed-length (remove #\Newline s))))

(defun day-09-part-2 (input-file)
  (let ((s (uiop:read-file-string input-file)))
    (decompressed-length (remove #\Newline s) :part2? t)))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2016 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
