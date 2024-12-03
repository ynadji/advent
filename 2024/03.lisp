(in-package :aoc2024)

(defparameter test-input-1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(defparameter test-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defun day-03-part-1 (input-file)
  (apply #'+ (mapcar (lambda (x) (apply #'* (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" x)))) (cl-ppcre:all-matches-as-strings "mul\\(\\d+,\\d+\\)" (uiop:read-file-string input-file)))))

(defun day-03-part-2 (input-file)
  (let* ((no-donts (loop with keep = t
                         for x in (cl-ppcre:all-matches-as-strings "(mul\\(\\d+,\\d+\\))|(don't\\(\\))|(do\\(\\))" (uiop:read-file-string input-file))
                         do (cond ((string= x "don't()") (setf keep nil))
                                  ((string= x "do()") (setf keep t)))
                         when (and keep (string/= x "do()")) collect x)))
    (apply #'+ (mapcar (lambda (x) (apply #'* (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" x)))) no-donts))))

(defun day-03 ()
  (let ((f (fetch-day-input-file 2024 3)))
    (values (day-03-part-1 f)
            (day-03-part-2 f))))
