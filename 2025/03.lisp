(in-package :aoc2025)

(defparameter test-input "987654321111111
811111111111119
234234234234278
818181911112111")

(defun parse-battery-banks (input-file)
  (map 'list (lambda (chars) (map 'list (lambda (c) (digit-char-p c)) chars))
       (uiop:read-file-lines input-file)))

(defun find-max-within (nums &optional (start 0) (end (1- (length nums))))
  (let ((max 0) (index -1))
   (loop for i from start below end for x in (subseq nums start end)
         when (> x max)
           do (setf max x index i)
         finally (return (values max index)))))

(defun day-03% (input-file num-digits)
  (loop for bank in (parse-battery-banks input-file)
        sum (loop for n from (1- num-digits) downto 0 with start-index = 0
                      for (max-n index-n) = (multiple-value-list (find-max-within bank start-index (- (length bank) n)))
                      do (setf start-index (1+ index-n))
                      sum (* max-n (expt 10 n)))))

(defun day-03 ()
  (let ((f (fetch-day-input-file 2025 3)))
    (values (day-03% f 2)
            (day-03% f 12))))
