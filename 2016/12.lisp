(in-package :aoc2016)

;; can i compile this somehow??

(defparameter test-input "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a")

(defun parse-instruction-line (line)
  (mapcar (lambda (x) (ax:if-let ((n (ignore-errors (parse-integer x)))) n (sintern x)))
          (str:split #\Space line)))

(defun run-assembunny (inst registers)
  (let ((op (first inst))
        (v1 (second inst)))
    (ecase op
      (|cpy| (if (numberp v1)
                 (setf (gethash (third inst) registers) v1)
                 (setf (gethash (third inst) registers) (gethash v1 registers))))
      (|inc| (incf (gethash v1 registers)))
      (|dec| (decf (gethash v1 registers)))
      (|jnz| (values (not (zerop (if (numberp v1) v1 (gethash v1 registers))))
                     (third inst))))))

(defun day-12% (input-file &optional (c 0))
  (let* ((registers (ax:alist-hash-table `((|a| . 0) (|b| . 0) (|c| . ,c) (|d| . 0))))
         (instructions (mapcar #'parse-instruction-line (uiop:read-file-lines input-file)))
         (instructions (make-array (length instructions) :initial-contents instructions))
         (pc 0))
    (loop while (array-in-bounds-p instructions pc)
          for (r1 r2) = (multiple-value-list (run-assembunny (aref instructions pc) registers))
          if (and r1 r2)
            do (incf pc r2)
          else
            do (incf pc 1)
          finally (return (values (gethash '|a| registers)
                                  registers)))))

(defun day-12 ()
  (let ((f (fetch-day-input-file 2016 12)))
    (values (day-12% f)
            (day-12% f 1))))
