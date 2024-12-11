(in-package :aoc2024)

(defun split-number (x)
  (let ((s (format nil "~a" x)))
    (values (parse-integer (subseq s 0 (/ (length s) 2)))
            (parse-integer (subseq s (/ (length s) 2))))))

(defun blink% (stone)
  (cond ((= 0 stone) 1)
        ((= 0 (mod (num-digits stone) 2)) (split-number stone))
        (t (* stone 2024))))

(defun blink (stones)
  (let ((next-stones (make-hash-table :size (hash-table-size stones))))
    (loop for stone being the hash-key of stones
          for new-stones = (multiple-value-list (blink% stone))
          do (loop for new-stone in new-stones
                   do (incf (gethash new-stone next-stones 0) (gethash stone stones))))
    next-stones))

(defun parse-stones (s)
  (let ((stones (make-hash-table)))
    (loop for stone in (mapcar #'parse-integer (str:split " " s))
          do (incf (gethash stone stones 0)))
    stones))

(defun count-stones (stones)
  (loop for freq being the hash-value of stones sum freq))

(defun stone-simulator (stones n)
  (count-stones (loop repeat n do (setf stones (blink stones)) finally (return stones))))

(defun day-11-part-1 (input-file)
  (let ((stones (parse-stones (uiop:read-file-string input-file))))
    (stone-simulator stones 25)))

(defun day-11-part-2 (input-file)
  (let ((stones (parse-stones (uiop:read-file-string input-file))))
    (stone-simulator stones 75)))

(defun day-11 ()
  (let ((f (fetch-day-input-file 2024 11)))
    (values (day-11-part-1 f)
            (day-11-part-2 f))))
