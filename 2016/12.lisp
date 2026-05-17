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

;; for day 23
(defun toggle (op)
  (ecase op
    (|inc| '|dec|)
    ((|dec| |tgl|) '|inc|)
    (|jnz| '|cpy|)
    (|cpy| '|jnz|)))

(defun run-assembunny (inst registers)
  (let ((op (first inst))
        (v1 (second inst)))
    (ecase op
      (|out| (if (numberp v1) v1 (values (gethash v1 registers)))) ; day 25
      (|tgl| (values (gethash v1 registers))) ; day 23
      (|cpy| (if (numberp v1)
                 (setf (gethash (third inst) registers) v1)
                 (setf (gethash (third inst) registers) (gethash v1 registers))))
      (|inc| (incf (gethash v1 registers)))
      (|dec| (decf (gethash v1 registers)))
      (|jnz| (values (not (zerop (if (numberp v1) v1 (gethash v1 registers))))
                     (let ((v2 (third inst)))
                       (if (numberp v2) v2 (gethash v2 registers))))))))

(defun alternating-1-0 (list)
  (loop for (x y) on list while y
        always (or (and (zerop x) (= 1 y))
                   (and (= 1 x) (zerop y)))))

(defun day-12% (input-file &key (a 0) (b 0) (c 0) (d 0) (min-output-length 10))
  (declare (optimize debug))
  (let* ((registers (ax:alist-hash-table `((|a| . ,a) (|b| . ,b) (|c| . ,c) (|d| . ,d))))
         (instructions (mapcar #'parse-instruction-line (uiop:read-file-lines input-file)))
         (instructions (make-array (length instructions) :initial-contents instructions))
         (pc 0)
         (output '())                   ; day 25
         )
    (loop while (array-in-bounds-p instructions pc)
          for (r1 r2) = (multiple-value-list (run-assembunny (aref instructions pc) registers))
          for op = (first (aref instructions pc))
          if (eq '|tgl| op)             ; day 23
            do (when (array-in-bounds-p instructions (+ pc r1))
                 (setf (first (aref instructions (+ pc r1))) (toggle (first (aref instructions (+ pc r1))))))
               (incf pc 1)
          else if (and r1 r2)
                 do (incf pc r2)
          else
            do (incf pc 1)
               (when (eq '|out| op) ; day 25
                 (push r1 output)
                 (if (alternating-1-0 output)
                     (when (> (length output) min-output-length)
                       (return output))
                     (return nil)))
          finally (return (values (gethash '|a| registers)
                                  registers)))))

(defun day-12 ()
  (let ((f (fetch-day-input-file 2016 12)))
    (values (day-12% f)
            (day-12% f :c 1))))
