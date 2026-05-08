(in-package :aoc2016)

(defun josephus-safe (n)
  (let ((larger-pow-2 (expt 2 (ceiling (log n) (log 2)))))
    (1+ (mod (ash n 1) larger-pow-2))))

(defun doophus-safe (n)
  (loop with x = 1 while (< (* x 3) n)
        do (setf x (* x 3))
        finally (return (- n x))))

(defun day-19-part-1 (input-file)
  (josephus-safe (parse-integer (uiop:read-file-string input-file))))

(defun day-19-part-2 (input-file)
  (doophus-safe (parse-integer (uiop:read-file-string input-file))))

(defun day-19 ()
  (let ((f (fetch-day-input-file 2016 19)))
    (values (day-19-part-1 f)
            (day-19-part-2 f))))
