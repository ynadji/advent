(in-package :aoc2024)

;(declaim (optimize (speed 3)))
;; can't get helpful optimizations just yet

(defparameter test-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defun fixnum-concat (x y)
  (declare (type fixnum x y))
  (+ (* x (expt 10 (num-digits y)))
     y))

(defun solve-equation (test-value vals &optional (acc 0) (part 1))
  (declare (type fixnum test-value acc part))
  (if (null vals)
      (= test-value acc)
      (or (solve-equation test-value (rest vals) (the fixnum (+ acc (first vals))) part)
          (solve-equation test-value (rest vals) (the fixnum (* acc (first vals))) part)
          (and (= part 2) (solve-equation test-value (rest vals) (fixnum-concat acc (first vals)) part)))))

(defun day-07% (input-file &optional (part 1))
  (let ((lines (uiop:read-file-lines input-file)))
    (loop for line in lines for vals = (mapcar #'parse-integer (str:split " " (str:replace-first ":" "" line)))
          when (solve-equation (first vals) (rest vals) 0 part)
            sum (first vals))))

(defun day-07 ()
  (let ((f (fetch-day-input-file 2024 7)))
    (values (day-07% f)
            (day-07% f 2))))
