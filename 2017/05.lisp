(in-package :aoc2017)

(defparameter test-input "0
3
0
1
-3")

(defun parse-jump-offsets (input-file)
  (coerce (mapcar #'parse-integer (uiop:read-file-lines input-file)) 'vector))

(defun run-jumps (jump-offsets &optional part2?)
  (loop with steps = 0 with i = 0
        unless (array-in-bounds-p jump-offsets i)
          do (return steps)
        do (cond ((or (not part2?) (< (aref jump-offsets i) 3))
                  (incf (aref jump-offsets i))
                  (incf i (1- (aref jump-offsets i)))
                  (incf steps))
                 (t
                  (decf (aref jump-offsets i))
                  (incf i (1+ (aref jump-offsets i)))
                  (incf steps)))))

(defun day-05% (input-file)
  (let ((jump-offsets (parse-jump-offsets input-file)))
    (values (run-jumps (copy-seq jump-offsets))
            (run-jumps jump-offsets t))))

(defun day-05 ()
  (let ((f (fetch-day-input-file 2017 5)))
    (day-05% f)))
