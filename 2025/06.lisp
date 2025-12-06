(in-package :aoc2025)

(defparameter test-input "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

(defun parse-math-homework (input-file)
  (mapcar #'reverse
          (transpose (loop for line in (uiop:read-file-lines input-file)
                           for nums = (string-to-num-list line)
                           if nums
                             collect nums
                           else
                             collect (mapcar (lambda (s) (symbol-function (symb s)))
                                             (str:split " " line :omit-nulls t))))))

(defun parse-cephalopod-math (input-file)
  (let ((s (uiop:read-file-string input-file))
        (ops (mapcar #'first (parse-math-homework input-file))))
    (loop for string-nums in (remove '("") (partition-by (mapcar (lambda (x) (remove-if-not #'digit-char-p x)) (transpose (str:split #\Newline (str:trim-right s :char-bag '(#\Newline))))) :f (lambda (x) (string= x ""))) :test #'equal)
          for nums = (mapcar #'parse-integer string-nums)
          for op in ops
          sum (apply op nums))))

(defun day-06-part-1 (input-file)
  (loop for math-problem in (parse-math-homework input-file)
        sum (apply (first math-problem) (rest math-problem))))

(defun day-06-part-2 (input-file)
  (parse-cephalopod-math input-file))

(defun day-06 ()
  (let ((f (fetch-day-input-file 2025 6)))
    (values (day-06-part-1 f)
            (day-06-part-2 f))))
