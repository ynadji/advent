(in-package :aoc2015)

(defun day-12-part-1 (input-file)
  (reduce #'+ (string-to-num-list (uiop:read-file-string input-file))))

(defun count-ignore-reds (json)
  (etypecase json
    (hash-table (let ((values (ax:hash-table-values json)))
                  (if (member "red" values :test #'equal)
                      0
                      (loop for x in values sum (count-ignore-reds x)))))
    (simple-vector (loop for x across json sum (count-ignore-reds x)))
    (integer json)
    (string 0)))

(defun day-12-part-2 (input-file)
  (let ((json (jzon:parse input-file)))
    (count-ignore-reds json)))

(defun day-12 ()
  (let ((f (fetch-day-input-file 2015 12)))
    (values (day-12-part-1 f)
            (day-12-part-2 f))))
