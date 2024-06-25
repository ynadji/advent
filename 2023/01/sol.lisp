(ql:quickload "str")

(defun find-calibration-value (string)
  (let* ((digits (loop for x across string when (digit-char-p x) collect x))
         (first (first digits))
         (last (first (last digits))))
    (parse-integer (format nil "~a~a" first last))))

(defun part-1 (input-file)
  (with-open-file (stream input-file)
    (loop for line = (read-line stream nil)
          while line
          sum (find-calibration-value line))))

(defvar *test-case-2*
  '("two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"))

(defun find-word (string)
  (cond ((str:starts-with? "one" string)   "1")
        ((str:starts-with? "two" string)   "2")
        ((str:starts-with? "three" string) "3")
        ((str:starts-with? "four" string)  "4")
        ((str:starts-with? "five" string)  "5")
        ((str:starts-with? "six" string)   "6")
        ((str:starts-with? "seven" string) "7")
        ((str:starts-with? "eight" string) "8")
        ((str:starts-with? "nine" string)  "9")
        (t                                 nil)))

(defun parse-string (string)
  (if (str:empty? string)
      nil
      (if (str:digit? (str:s-first string))
          (cons (str:s-first string)
                (parse-string (str:s-rest string)))
          (let ((digit (find-word string)))
            (if digit
                (cons digit (parse-string (str:substring 1 nil string)))
                (parse-string (str:substring 1 nil string)))))))

(defun find-calibration-value-2 (string)
  (let* ((digits (parse-string string))
         (first (first digits))
         (last (first (last digits))))
    (parse-integer (format nil "~a~a" first last))))

(defun part-2 (input-file)
  (with-open-file (stream input-file)
    (loop for line = (read-line stream nil)
          while line
          sum (find-calibration-value-2 line))))
