(in-package :aoc2016)

(defun generate-next-row (row)
  (flet ((trap? (l c r)
           (or (and (char= l #\^) (char= c #\^) (char= r #\.))
               (and (char= l #\.) (char= c #\^) (char= r #\^))
               (and (char= l #\^) (char= c #\.) (char= r #\.))
               (and (char= l #\.) (char= c #\.) (char= r #\^)))))
    (coerce (loop for (left center right) on (coerce (concatenate 'string "." row ".") 'list)
                  while right
                  collect (if (trap? left center right) #\^ #\.))
            'string)))

(defun count-safe-tiles (first-row total-rows)
  (loop with row = first-row repeat total-rows
        sum (count #\. row)
        do (setf row (generate-next-row row))))

(defun day-18% (input-file)
  (let ((first-row (uiop:stripln (uiop:read-file-string input-file))))
    (values (count-safe-tiles first-row 40)
            (count-safe-tiles first-row 400000))))

(defun day-18 ()
  (let ((f (fetch-day-input-file 2016 18)))
    (day-18% f)))
