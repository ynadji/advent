(defparameter test-input "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i")

(defun lshift (x n) (ash x n))
(defun rshift (x n) (ash x (- n)))
(defun bitwise-not (x) (mod (lognot x) (expt 2 16)))

(defparameter op->fun `((AND . ,#'logand) (OR . ,#'logior) (LSHIFT . ,#'lshift) (RSHIFT . ,#'rshift)))

(defun parse-wire-field (x)
  (if (every #'digit-char-p x)
      (parse-integer x)
      (intern x)))

(define-condition missing-value (error) ())

(defun get-wire-val (ht x)
  (if x
      (if (symbolp x)
          (get-wire-val ht (gethash x ht))
          x)
      (error 'missing-value)))

;; uhh not quite sure what you're doing here anymore.
(defun (setf get-wire-val) (new-val ht x)
  (setf (gethash x ht) (get-wire-val ht x)))

(defun update-wires (ht line)
  (let ((parsed (mapcar #'parse-wire-field (remove-if (lambda (s) (string= s "->")) (str:split " " line)))))
    (ecase (length parsed)
      (2 (setf (gethash (second parsed) ht)
               (get-wire-val ht (first parsed))))
      (3 (setf (gethash (third parsed) ht)
               (bitwise-not (get-wire-val ht (second parsed)))))
      (4 (setf (gethash (fourth parsed) ht)
               (funcall (ax:assoc-value op->fun (second parsed))
                        (gethash (first parsed) ht)
                        (if (or (eq 'LSHIFT (second parsed))
                                (eq 'RSHIFT (second parsed)))
                            (third parsed)
                            (get-wire-val ht (third parsed)))))))))

(defun run-wires (ht lines)
  (loop for line in lines
        for res = (handler-case (update-wires ht line)
                    (missing-value (c) ;;(format t "~a~%line: ~a~%" c line)
                           (values)))
        unless res
        collect line))

(defun day-7-part-1 (input-file)
  (let ((ht (make-hash-table))
        (lines (uiop:read-file-lines input-file)))
    (loop while lines
          do (format t "~a " (length lines))
          (setf lines (run-wires ht lines))
          when (= 319 (length lines))
          do (return (values ht lines)))))
