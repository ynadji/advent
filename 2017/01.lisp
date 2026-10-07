(in-package :aoc2017)

(defparameter test-inputs '("1122" "1111" "1234" "91212129"))

(defun inverse-captcha (s &optional (delta 1))
  (loop for i below (1- (length s))
        when (char= (char s i) (char s (mod (- i delta) (length s))))
          sum (digit-char-p (char s i))))

(defun day-01 ()
  (let ((s (uiop:stripln (uiop:read-file-string (fetch-day-input-file 2017 1)))))
    (values (inverse-captcha s)
            (inverse-captcha s (/ (length s) 2)))))
