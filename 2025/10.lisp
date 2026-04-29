(in-package :aoc2025)

(defparameter test-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defclass machine ()
  ((goal :accessor goal :initarg :goal)
   (buttons :accessor buttons :initarg :buttons)
   (joltage :accessor joltage :initarg :joltage)
   (button-list :accessor button-list :initarg :button-list)))

(defmethod print-object ((m machine) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (goal buttons joltage button-list) m
      (format stream "~a ~a ~a ~a" goal buttons joltage button-list))))

(defun parse-machine (line)
  (flet ((parse-lights (s)
           (parse-integer (substitute #\0 #\. (substitute #\1 #\# (str:trim s :char-bag '(#\[ #\]))))
                          :radix 2))
         (parse-button (lights elements)
           (let ((s (make-string (- (length lights) 2) :initial-element #\0)))
             (loop for i in elements do (setf (char s i) #\1))
             (parse-integer s :radix 2))))
    (let* ((chunks (str:split #\Space line))
           (goal (parse-lights (first chunks)))
           (joltage (ax:lastcar chunks))
           (buttons (subseq chunks 1 (1- (length chunks))))
           (button-list (mapcar #'string-to-num-list buttons))
           (buttons (mapcar (lambda (elts) (parse-button (first chunks) elts)) button-list)))
      (make-instance 'machine :goal goal :buttons buttons :joltage (string-to-num-list joltage)
                              :button-list button-list))))

(defun parse-machines (input-file)
  (mapcar #'parse-machine (uiop:read-file-lines input-file)))

(defun minimum-presses (machine)
  (loop for button-set in (sort (powerset (buttons machine)) #'< :key #'length)
        when (= (goal machine) (apply #'logxor 0 button-set))
          return (length button-set)))

(defun day-10-part-1 (input-file)
  (let ((machines (parse-machines input-file)))
    (loop for i from 0
          for machine in machines sum (minimum-presses machine))))

(defun generate-linear-problem (joltage button-list)
  (values `(min (= n (+ ,@(loop for i from 1 for b in button-list collect (symb 'b i)))))
          `((integer ,@(loop for b from 1 for buttons in button-list collect (symb 'b b)))
            ,@(loop for jolt in joltage for i from 0
                    collect
                    `(= ,jolt
                        (+ ,@(loop for b from 1 for buttons in button-list
                                   when (member i buttons)
                                     collect (symb 'b b))))))))

;; fails with/without filtering the redundant constraints :(
;;(setf linear-programming:*solver* linear-programming-glpk:glpk-solver)

;; NB: Assumes https://github.com/neil-lindquist/linear-programming/pull/16 has been merged.
(defun day-10-part-2 (input-file)
  (let ((machines (parse-machines input-file)))
    (loop for machine in machines
          for (objective constraints) = (multiple-value-list (generate-linear-problem (joltage machine)
                                                                                      (button-list machine)))
          sum (lp:solution-variable (lp:solve-problem (lp:parse-linear-problem objective constraints)) 'N))))

(defun day-10 ()
  (let ((f (fetch-day-input-file 2025 10)))
    (values (day-10-part-1 f)
            (day-10-part-2 f))))
