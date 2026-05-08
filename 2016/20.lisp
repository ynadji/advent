(in-package :aoc2016)

(defparameter test-input "5-8
0-2
4-7")

(defun parse-ips (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        collect (mapcar #'parse-integer (str:split #\- line))))

(defun day-20% (blocked-ip-ranges)
  (let* ((sorted-ip-ranges (sort blocked-ip-ranges #'< :key #'first))
         (prev-start (first (first sorted-ip-ranges)))
         (prev-end (second (first sorted-ip-ranges)))
         (blocked-ips 0)
         (first-allowed))
    (loop for (start end) in (rest sorted-ip-ranges)
          if (or (= (1+ prev-end) start)
                 (<= prev-start start prev-end))
            do (setf prev-end (max end prev-end))
          else
            do (unless first-allowed
                 (setf first-allowed (1+ prev-end)))
               (incf blocked-ips (1+ (- prev-end prev-start)))
               (setf prev-start start prev-end end)
          finally (incf blocked-ips (1+ (- (max end prev-end) (min start prev-start)))))
    (values first-allowed
            (- (expt 2 32) blocked-ips))))

(defun day-20 ()
  (let ((f (fetch-day-input-file 2016 20)))
    (day-20% (parse-ips f))))
