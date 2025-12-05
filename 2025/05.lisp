(in-package :aoc2025)

(defparameter test-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defun parse-ingredient-db (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        if (str:contains? "-" line)
          collect (string-to-num-list (substitute #\Space #\- line)) into db
        else
          unless (string= "" line)
            collect (parse-integer line) into available
        finally (return (values db available))))

(defun day-05-part-1 (input-file)
  (multiple-value-bind (db available-ingredients) (parse-ingredient-db input-file)
    (loop for available in available-ingredients
          count (loop for (start end) in db
                      thereis (<= start available end)))))

(defun range-overlap? (r1 r2)
  (when (and r1 r2)
    (not (or (< (second r1) (first r2)) (< (second r2) (first r1))))))

(defun merge-ranges (r1 r2)
  (when (range-overlap? r1 r2)
    (list (min (first r1) (first r2))
          (max (second r1) (second r2)))))

(defun shrink-ranges (ranges)
  (loop for i from 0 for r1 in ranges
        do (loop for j from (1+ i) for r2 in (subseq ranges j)
                 when (range-overlap? r1 r2)
                   do (let ((new-r1 (merge-ranges r1 r2)))
                        (setf (nth i ranges) (merge-ranges r1 r2)
                              (nth j ranges) nil
                              r1 new-r1))))
  (remove nil ranges))

(defun shrink-ranges-until-converges (ranges)
  (loop with prev-length = (length ranges)
        do (setf ranges (shrink-ranges ranges))
        until (= prev-length (length ranges))
        do (setf prev-length (length ranges))
        finally (return ranges)))

(defun day-05-part-2 (input-file)
  (let ((ranges (parse-ingredient-db input-file)))
    (loop for (start end) in (shrink-ranges-until-converges ranges)
          sum (1+ (- end start)))))

(defun day-05 ()
  (let ((f (fetch-day-input-file 2025 5)))
    (values (day-05-part-1 f)
            (day-05-part-2 f))))
