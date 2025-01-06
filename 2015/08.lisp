(in-package :aoc2015)

(defun encoding-lengths (s)
  (let ((s-unhex (remove #\\ (str:replace-all "\\\\" "!"
                                              (str:replace-all "\\\\\x[0-9a-f]{2}" "!" s :regex t)))))
    (list (length s)
          (- (length s-unhex) 2) ; subtract leading/trailing \"
          (length (with-output-to-string (stream) (prin1 s stream))))))

(defun day-08 ()
  (let ((f (fetch-day-input-file 2015 8)))
    (loop for s in (uiop:read-file-lines f)
          for (code memory encoded) = (encoding-lengths s)
          sum (- code memory) into part1
          sum (- encoded code) into part2
          finally (return (values part1 part2)))))
