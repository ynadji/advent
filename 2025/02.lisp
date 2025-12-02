(in-package :aoc2025)

(defparameter test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(defun parse-id-ranges (input-file)
  (mapcar (lambda (x) (mapcar #'parse-integer (str:split #\- x))) (str:split #\, (uiop:read-file-string input-file))))

(defun invalid-id? (id &optional part2?)
  (let ((id (coerce id 'list)))
    (loop for split from 1 upto (/ (length id) 2)
          for groups = (group id split)
            thereis (and (loop for (x y) on groups
                               while (and x y)
                               always (equal x y))
                         (or part2? (evenp (length groups)))))))

(defun day-02% (input-file &optional part2?)
  (let ((id-ranges (parse-id-ranges input-file)))
    (loop for (start end) in id-ranges
          sum (loop for id from start upto end
                    when (invalid-id? (format nil "~a" id) part2?)
                      sum id))))

(defun day-02 ()
  (let ((f (fetch-day-input-file 2025 2)))
    (values (day-02% f)
            (day-02% f t))))
