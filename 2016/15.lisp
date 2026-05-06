(in-package :aoc2016)

(defparameter test-input "Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.")

(defun parse-discs (input-str)
  (loop for line in (str:split #\Newline input-str :omit-nulls t)
        collect (string-to-num-list line)))

(defun find-first-capsule-time (discs)
  (declare (optimize speed))
  (flet ((satisfies? (discs)
           (every (lambda (disc) (zerop (the fixnum (fourth disc)))) discs))
         (mod+ (disc delta)
           (declare (type fixnum delta))
           (the fixnum (mod (the fixnum (+ delta (the fixnum (fourth disc)))) (the fixnum (second disc))))))
    ;; initialize for releasing at t=0
    (loop for x from 1 for disc in discs
          do (setf (fourth disc) (mod+ disc x)))
    (loop for time from 0 when (satisfies? discs) do (return time)
          do (loop for disc in discs do (setf (fourth disc) (mod+ disc 1))))))

(defun day-15-part-1 (input-file)
  (find-first-capsule-time (parse-discs (uiop:read-file-string input-file))))

(defun day-15-part-2 (input-file)
  (find-first-capsule-time (append (parse-discs (uiop:read-file-string input-file))
                                   (list '(7 11 0 0)))))

(defun day-15 ()
  (let ((f (fetch-day-input-file 2016 15)))
    (values (day-15-part-1 f)
            (day-15-part-2 f))))
