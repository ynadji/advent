(in-package :aoc2015)

(defparameter test-input "inc a
jio a, +2
tpl a
inc a")

(defparameter op->fun `((|hlf| . ,#'(lambda (x) (/ x 2)))
                        (|tpl| . ,#'(lambda (x) (* x 3)))
                        (|inc| . ,#'(lambda (x) (1+ x)))
                        (|jmp| . ,#'(lambda (x) (parse-integer (string x))))
                        (|jie| . ,#'(lambda (r o) (if (zerop (mod r 2)) (parse-integer (string o)) 1)))
                        (|jio| . ,#'(lambda (r o) (if (= r 1) (parse-integer (string o)) 1)))))

(defun parse-instructions (input-file)
  (let ((insts (loop for line in (uiop:read-file-lines input-file)
                     collect (mapcar #'sintern (cl-ppcre:split ", | " line)))))
    (make-array (length insts) :initial-contents insts)))

(defun run-instructions (instructions &optional (a 0))
  (let ((regs (list (cons '|a| a) (cons '|b| 0))))
    (loop with ir = 0 while (array-in-bounds-p instructions ir)
          for op-args = (aref instructions ir)
          for op = (first op-args)
          for args = (rest op-args)
          for arg-vals = (mapcar (lambda (x) (ax:if-let ((val (ax:assoc-value regs x))) val x)) args)
          for func = (ax:assoc-value op->fun op)
          for res = (funcall #'apply func arg-vals)
          if (member op '(|jmp| |jie| |jio|))
            do (incf ir res)
          else
            do (setf (ax:assoc-value regs (first args)) res)
               (incf ir))
    (ax:assoc-value regs '|b|)))

(defun day-23-part-1 (input-file)
  (run-instructions (parse-instructions input-file)))

(defun day-23-part-2 (input-file)
  (run-instructions (parse-instructions input-file) 1))

(defun day-23 ()
  (let* ((f (fetch-day-input-file 2015 23))
         (instructions (parse-instructions f)))
    (values (run-instructions instructions)
            (run-instructions instructions 1))))
