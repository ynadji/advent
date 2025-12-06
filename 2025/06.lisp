(in-package :aoc2025)

(defparameter test-input "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

(defun parse-math-homework (input-file)
  (flet ((string-to-func (s) (symbol-function (symb s))))
    (let ((math-rows (loop for line in (uiop:read-file-lines input-file)
                           for nums = (string-to-num-list line)
                           if nums
                             collect nums
                           else
                             collect (mapcar #'string-to-func (str:split-omit-nulls " " line)))))
      (mapcar #'reverse (transpose math-rows)))))

(defun parse-cephalopod-math (input-file)
  (flet ((chomp (s) (str:trim-right s :char-bag '(#\Newline)))
         (remove-non-digits (list) (remove-if-not #'digit-char-p list))
         (drop-empty-string-list (list) (remove '("") list :test #'equal))
         (partition-by-empty-string (list) (partition-by list :f (lambda (x) (string= x "")))))
    (loop for string-nums in (->> input-file
                               uiop:read-file-string
                               chomp
                               (str:split #\Newline)
                               transpose
                               (mapcar #'remove-non-digits)
                               partition-by-empty-string
                               drop-empty-string-list)
          for nums = (mapcar #'parse-integer string-nums)
          for op in (mapcar #'first (parse-math-homework input-file))
          collect (cons op nums))))

(defun day-06% (input-file parse-function)
  (loop for math-problem in (funcall parse-function input-file)
        sum (apply (first math-problem) (rest math-problem))))

(defun day-06 ()
  (let ((f (fetch-day-input-file 2025 6)))
    (values (day-06% f #'parse-math-homework)
            (day-06% f #'parse-cephalopod-math))))
