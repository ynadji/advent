(in-package :aoc2023)

(defun light-reachable? (M pos dir)
  (let ((c (aref M (car pos) (cdr pos)))
        (in-dir (opposite-direction dir)))
    (cond ((char= c #\.) t)
          (t in-dir))))

(defun next-directions (c dir)
  (case c
    (#\. (list dir))
    (#\| (if (or (eq dir :west) (eq dir :east))
             (list :north :south)
             (list dir)))
    (#\- (if (or (eq dir :north) (eq dir :south))
             (list :east :west)
             (list dir)))
    (#\/ (case dir (:north '(:east)) (:east '(:north)) (:south '(:west)) (:west '(:south))))
    (#\\ (case dir (:north '(:west)) (:west '(:north)) (:south '(:east)) (:east '(:south))))))

(defun pew-pew (grid start direction)
  (let ((visited (make-hash-table :test #'equal :size 4096))
        to-visit directions)
    (dolist (nd (next-directions (aref grid (car start) (cdr start)) direction))
      (push start to-visit)
      (push nd directions))
    (loop while (and to-visit directions) do
      (let* ((curr-pos (pop to-visit))
             (dir (pop directions))
             (pos-dir (cons curr-pos dir)))
        (when (not (gethash pos-dir visited))
          (setf (gethash pos-dir visited) t)
          (multiple-value-bind (next-positions next-directions) (2d-neighbors grid (car curr-pos) (cdr curr-pos) :wanted-directions (list dir))
            (loop for np in next-positions for nds in next-directions
                  ;; NEXT-DIRECTIONS can return multiple values, e.g., from
                  ;; #\- and #\| splitters.
                  do (dolist (nd (next-directions (aref grid (car np) (cdr np)) nds))
                       (push np to-visit)
                       (push nd directions)))))))
    (-<>> visited
      ax:hash-table-keys
      (mapcar #'car)
      (remove-duplicates <> :test #'equal)
      length)))

(defun day-16-part-1 (input-file)
  (-> input-file read-maze (pew-pew '(0 . 0) :east)))

(defun border-starts (grid)
  (let ((nrows (1- (array-dimension grid 0)))
        (ncols (1- (array-dimension grid 1))))
    (append (loop for j upto ncols
                  collect (cons (cons 0 j) :south)
                  collect (cons (cons nrows j) :north))
            (loop for i upto nrows
                  collect (cons (cons i 0) :east)
                  collect (cons (cons i ncols) :west)))))

(defun day-16-part-2 (input-file)
  (let ((grid (read-maze input-file)))
    (loop for (pos . dir) in (border-starts grid)
          maximize (pew-pew grid pos dir))))

(defun day-16 ()
  (let ((f (fetch-day-input-file 2023 16)))
    (values (day-16-part-1 f)
            (day-16-part-2 f))))
