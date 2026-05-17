(in-package :aoc2016)

(defparameter test-input "$ df -h
Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%")

(defun parse-drives (input-file)
  (coerce (loop for df in (cddr (uiop:read-file-lines input-file))
                collect (string-to-num-list (remove #\- df)))
          'vector))

(defun count-viable-pairs (drives)
  (loop for i from 0 below (length drives)
        sum (loop for j from 0 below (length drives)
                  for a = (aref drives i)
                  for b = (aref drives j)
                  count (and (/= i j)
                             (plusp (fourth a))
                             (< (fourth a) (fifth b))))))

(defun day-22-part-1 (input-file)
  (count-viable-pairs (parse-drives input-file)))

(defun day-22-part-2 (input-file) (progn input-file -1))

(defun day-22 ()
  (let ((f (fetch-day-input-file 2016 22)))
    (values (day-22-part-1 f)
            (day-22-part-2 f))))
