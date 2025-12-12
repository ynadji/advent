(in-package :aoc2025)

(defparameter test-input "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")

(defun parse-present (lines)
  (let ((index (parse-integer (first lines) :junk-allowed t)))
    (values index (parse-grid (str:join #\Newline (rest lines))))))

(defstruct region grid presents)

(defun parse-region (line)
  (let ((nums (string-to-num-list line)))
    (make-region :grid (make-array (reverse (subseq nums 0 2)) :element-type 'standard-char :initial-element #\.)
                 :presents (loop for i from 0 for num in (rest (rest nums))
                                 collect (cons i num)))))

(defun parse-presents-and-regions (input-file)
  (let ((presents (make-hash-table))
        buf)
    (values presents
            (loop for line in (uiop:read-file-lines input-file)
                  if (str:contains? "x" line)
                    collect (parse-region line)
                  else
                    do (if (string= line "")
                           (multiple-value-bind (index grid) (parse-present (reverse buf))
                             (setf (gethash index presents) grid)
                             (setf buf nil))
                           (push line buf))))))

(defun day-12-part-1 (input-file) (progn input-file -1))

(defun day-12-part-2 (input-file) (progn input-file -1))

(defun day-12 ()
  (let ((f (fetch-day-input-file 2025 12)))
    (values (day-12-part-1 f)
            (day-12-part-2 f))))
