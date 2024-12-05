(in-package :aoc2024)

(defparameter test-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defvar *ht* nil)

(defun sorter (x y)
  (if y (member y (gethash x *ht*)) t))

(defun sorted? (list)
  (loop for (x y) on list always (sorter x y)))

(defun middle (list)
  (nth (floor (/ (length list) 2)) list))

(defun parse-05 (input-file)
  (let ((lines (uiop:read-file-lines input-file))
        (ht (make-hash-table))
        updates)
    (loop for line in lines with order? = t do
      (cond ((string= line "") (setf order? nil))
            (order? (cl-ppcre:register-groups-bind ((#'parse-integer x y)) ("(\\d+)\\|(\\d+)" line)
                      (push y (gethash x ht))))
            (t (push (mapcar #'parse-integer (str:split "," line)) updates))))
    (values ht updates)))

(defun day-05-part-1 (input-file)
  (multiple-value-bind (*ht* updates) (parse-05 input-file)
    (loop for update in updates when (sorted? update) sum (middle update))))

(defun day-05-part-2 (input-file)
  (multiple-value-bind (*ht* updates) (parse-05 input-file)
    (loop for update in updates
          unless (sorted? update)
            sum (middle (sort update #'sorter)))))

(defun day-05 ()
  (let ((f (fetch-day-input-file 2024 5)))
    (values (day-05-part-1 f)
            (day-05-part-2 f))))
