(in-package :aoc2024)

(defparameter test-input "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

;; If register C contains 9, the program 2,6 would set register B to 1.
;; If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
;; If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
;; If register B contains 29, the program 1,7 would set register B to 26.
;; If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.

(defvar *a* 0)
(defvar *b* 0)
(defvar *c* 0)
(defvar *ip* 0)
(defvar *output* nil)

(defun read-program (input-file)
  (destructuring-bind (a b c &rest instructions)
      (ax:flatten (mapcar #'string-to-num-list (uiop:read-file-lines input-file)))
    (values a b c (make-array (length instructions) :element-type 'fixnum :initial-contents instructions))))

(defun combo (cop)
  (ecase cop
    ((0 1 2 3) cop)
    (4 *a*)
    (5 *b*)
    (6 *c*)))

(defun adv (cop)
  (let ((cop (combo cop)))
    (setf *a* (floor (/ *a* (expt 2 cop))))))

(defun bxl (op)
  (setf *b* (logxor *b* op)))

(defun bst (cop)
  (let ((cop (combo cop)))
    ;;(format t "setf *b* ~a~%" (mod cop 8))
    (setf *b* (mod cop 8))))

(defun jnz (op)
  ;;(format t "jnz ~a, *a*: ~a~%" op *a*)
  (unless (zerop *a*)
    (setf *ip* op)))

(defun bxc (op)
  (declare (ignore op))
  (setf *b* (logxor *b* *c*)))

(defun out (cop)
  (let ((cop (combo cop)))
    (push (mod cop 8) *output*)))

(defun bdv (cop)
  (let ((cop (combo cop)))
    (setf *b* (floor (/ *a* (expt 2 cop))))))

(defun cdv (cop)
  (let ((cop (combo cop)))
    (setf *c* (floor (/ *a* (expt 2 cop))))))

(defvar op->fun '((0 . adv) (1 . bxl) (2 . bst) (3 . jnz) (4 . bxc) (5 . out) (6 . bdv) (7 . cdv)))

(defun run-program (instructions)
  (let ((*output* nil))
   (loop while (array-in-bounds-p instructions *ip*)
         for func = (ax:assoc-value op->fun (aref instructions *ip*))
         for operand = (aref instructions (1+ *ip*))
         for res = (funcall func operand)
         do (format t "~2,' d (FUNCALL ~a ~a)~%" *ip* func operand)
         unless (and (eq func 'jnz) res)
           do (incf *ip* 2))
    (format nil "~{~a~^,~}" (reverse *output*))))

;; guessed 1,7,7,0,5,7,3,2,7
(defun day-17-part-1 (input-file)
  (multiple-value-bind (a b c instructions) (read-program input-file)
    (let ((*a* a) (*b* b) (*c* c) (*ip* 0) *output*)
      (run-program instructions))))

(defun day-17-part-2 (input-file) (progn input-file -1))

(defun day-17 ()
  (let ((f (fetch-day-input-file 2024 17)))
    (values (day-17-part-1 f)
            (day-17-part-2 f))))
