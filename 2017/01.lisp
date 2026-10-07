(in-package :aoc2017)

(defparameter test-inputs '("1122" "1111" "1234" "91212129"))

(defun inverse-captcha (s)
  (let ((prev (char s (1- (length s)))))
    (loop for c across s when (char= c prev) sum (digit-char-p c)
          do (setf prev c))))

(defun day-01-part-1 (input-file)
  (inverse-captcha (uiop:stripln (uiop:read-file-string input-file))))

(defun inverse-captcha-halfway (s)
  (loop for i below (length s) with half-length = (/ (length s) 2)
        when (char= (char s i) (char s (mod (+ i half-length) (length s))))
          sum (digit-char-p (char s i))))

(defun day-01-part-2 (input-file)
  (inverse-captcha-halfway (uiop:stripln (uiop:read-file-string input-file))))

(defun day-01 ()
  (let ((f (fetch-day-input-file 2017 1)))
    (values (day-01-part-1 f)
            (day-01-part-2 f))))
