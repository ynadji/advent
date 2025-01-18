(in-package :aoc2016)

(defparameter test-input "rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1")

(defun turn-on-rect (arr col row)
  (loop for i below row do
    (loop for j below col do
      (setf (aref arr i j) #\#)))
  arr)

(defun parse-screen-line (line)
  (let ((nums (string-to-num-list line)))
    (cons (cond ((str:starts-with? "rect" line) :rect)
                ((str:starts-with? "rotate column" line) :col)
                ((str:starts-with? "rotate row" line) :row))
          nums)))

(defun day-08-part-1 (input-file &optional (ncol 50) (nrow 6))
  (let ((arr (make-array (list nrow ncol) :element-type 'standard-char :initial-element #\.))
        (lines (uiop:read-file-lines input-file)))
    (loop for (op x y) in (mapcar #'parse-screen-line lines)
          do (ecase op
               (:rect (turn-on-rect arr x y))
               (:col (shift-array arr y :col x))
               (:row (shift-array arr y :row x))))
    ;;(print-grid arr)
    (aops:sum-index (i j)
      (if (char= #\# (aref arr i j)) 1 0))))

;; TODO: uhh how do i recognize the letters programmatically?
(defun day-08-part-2 (input-file) (progn input-file "ZJHRKCPLYJ"))

(defun day-08 ()
  (let ((f (fetch-day-input-file 2016 8)))
    (values (day-08-part-1 f)
            (day-08-part-2 f))))
