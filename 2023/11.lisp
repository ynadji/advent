(in-package :aoc2023)

(defun expand-galaxy-rows (lines)
  (loop for line in lines
        append (if (every (lambda (c) (char= #\. c)) line)
                   (list line line)
                   (list line))))

(defun find-galaxy-expander-intercepts (lines)
  (loop for line in lines for i from 0
        when (every (lambda (c) (char= #\. c)) line)
          collect i))

(defun expand-galaxy (lines)
  "Expand the galaxy represented by LINES. Rather than mucking about with expanding
columns, just transpose the lines like a matrix to treat the columns like rows."
  (-> lines
    (expand-galaxy-rows)
    (transpose)
    (expand-galaxy-rows)
    (transpose)))

(defun expansion-intercepts (lines)
  "Find the row and column indices, which correspond to the x and y intercepts,
respectively, that would need to be enlarged because they have no galaxies."
  (let ((xints (find-galaxy-expander-intercepts lines))
        (yints (find-galaxy-expander-intercepts (transpose lines))))
    (values xints yints)))

(defun num-intercept-crosses (p1 p2 xints yints)
  (destructuring-bind ((x1 . y1) (x2 . y2)) (list p1 p2)
    (+ (count-if (lambda (xint) (< (min x1 x2) xint (max x1 x2))) xints)
       (count-if (lambda (yint) (< (min y1 y2) yint (max y1 y2))) yints))))

(defun manhattan-distance (p1 p2 &key (xints nil) (yints nil) (expansion-factor 0))
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cdr p1) (cdr p2)))
     (* expansion-factor (num-intercept-crosses p1 p2 xints yints))))

(defun read-galaxy (input-file &key (skip-expand? nil))
  (let* ((lines (if skip-expand?
                    (-> input-file uiop:read-file-lines)
                    (-> input-file uiop:read-file-lines expand-galaxy)))
         (nrows (length lines))
         (ncols (length (first lines)))
         (galaxy-map (make-array (list nrows ncols) :initial-element #\.))
         (galaxy-positions))
    (loop for row in lines for i from 0 do
      (loop for x across row for j from 0 do
        (when (eq x #\#)
          (setf (aref galaxy-map i j) #\#)
          (push (cons i j) galaxy-positions))))
    (values galaxy-map
            (make-array (length galaxy-positions) :initial-contents galaxy-positions))))

(defun sum-min-distances (galaxy-positions &key (xints nil) (yints nil) (expansion-factor 0))
  "Loop over pairwise combinations to compute distances."
  (loop for i below (length galaxy-positions)
        sum (loop for j from (1+ i) below (length galaxy-positions)
                  sum (manhattan-distance (aref galaxy-positions i)
                                          (aref galaxy-positions j)
                                          :xints xints :yints yints
                                          :expansion-factor expansion-factor))))

;; TODO: we don't actually ever need the map, so just return the positions.
(defun day-11-part-1 (input-file)
  (multiple-value-bind (galaxy-map galaxy-positions) (read-galaxy input-file)
    (declare (ignore galaxy-map))
    (sum-min-distances galaxy-positions)))

(defun day-11-part-2 (input-file)
  (multiple-value-bind (galaxy-map galaxy-positions) (read-galaxy input-file :skip-expand? t)
    (declare (ignore galaxy-map))
    (multiple-value-bind (xints yints) (expansion-intercepts (uiop:read-file-lines input-file))
      ;; TODO: is the off-by-one because we already include the non-expanded
      ;; empty cols/rows?
      (sum-min-distances galaxy-positions :xints xints :yints yints :expansion-factor 999999))))

(defun day-11 ()
  (let ((f #p"11-input.txt"))
    (values (day-11-part-1 f)
            (day-11-part-2 f))))
