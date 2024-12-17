(in-package :aoc2024)

(defparameter test-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defun count-xmas-samx (string)
  (+ (cl-ppcre:count-matches "XMAS" string)
     (cl-ppcre:count-matches "SAMX" string)))

(defun get-diagonals (seqs &optional (n 1))
  (if (null (ax:lastcar seqs))
      nil
      (let ((relevant (subseq seqs 0 n)))
        (cons (remove nil (mapcar #'first relevant))
              (get-diagonals (append (mapcar #'rest relevant)
                                     (subseq seqs n))
                             (min (1+ n) (length seqs)))))))

(defun day-04-part-1 (input-file)
  (let ((lines (uiop:read-file-lines input-file)))
    (+ (apply #'+ (mapcar #'count-xmas-samx lines))
       (apply #'+ (mapcar #'count-xmas-samx (transpose lines)))
       (apply #'+ (mapcar #'count-xmas-samx (mapcar #'chars-to-string (get-diagonals (mapcar #'string-to-chars lines)))))
       (apply #'+ (mapcar #'count-xmas-samx (mapcar #'chars-to-string (get-diagonals (mapcar #'string-to-chars (rotate-90-clockwise lines)))))))))

(defun x-mas (grid i j)
  (and (eq #\A (aref grid i j))
       (let ((cross (loop for (i . j) in (2d-neighbors grid (cons i j) :wanted-directions (apply #'directions->deltas *inter-cardinals*))
                          collect (aref grid i j))))
         (or (equal cross '(#\S #\S #\M #\M))
             (equal cross '(#\M #\M #\S #\S))
             (equal cross '(#\S #\M #\M #\S))
             (equal cross '(#\M #\S #\S #\M))))))

(defun day-04-part-2 (input-file)
  (let ((grid (read-grid input-file)))
    (loop for i from 1 below (1- (array-dimension grid 0))
          sum (loop for j from 1 below (1- (array-dimension grid 1))
                    count (x-mas grid i j)))))

(defun day-04 ()
  (let ((f (fetch-day-input-file 2024 4)))
    (values (day-04-part-1 f)
            (day-04-part-2 f))))
