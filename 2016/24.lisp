(in-package :aoc2016)

(defparameter test-input "###########
#0.1.....2#
#.#######.#
#4.......3#
###########")

(defvar *grid* (parse-grid test-input :starts? #'digit-char-p))

(defun 24-reachable? (m pos dir)
  (declare (ignore dir))
  (char/= (paref m pos) #\#))

(defun number-char-over-0? (c)
  (and (digit-char-p c)
       (char/= c #\0)))

(defun points-of-interest ()
  (let ((nums '()))
    (aops:each-index (i j)
      (when (digit-char-p (aref *grid* i j))
        (push (aref *grid* i j) nums)))
    nums))

(defun 24-state= (s1 s2)
  (and (equal (car s1) (car s2))
       (equal (caddr s1) (caddr s2))))

(sb-ext:define-hash-table-test 24-state= (lambda (s) (sxhash `(,(car s) (caddr s)))))

(defun 24-successors (state)
  (multiple-value-bind (positions directions)
      (2d-neighbors *grid* (caddr state) :reachable? #'24-reachable?)
    (declare (ignorable directions))
    (loop with seen-numbers = (car state)
          with new-steps = (1+ (cadr state))
          for pos in positions for c = (paref *grid* pos)
          if (number-char-over-0? c)
            collect (list (adjoin c seen-numbers) new-steps pos)
          else
            collect (list seen-numbers new-steps pos))))

(defun generate-possible-paths (digits)
  (remove-if-not (equals #\0) (permutations digits) :key #'first))

(defun pairwise-shortest-path-lengths (number-positions)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (x y) in (combinations number-positions 2)
          do (let ((xy (cons (paref *grid* x) (paref *grid* y)))
                   (yx (cons (paref *grid* y) (paref *grid* x)))
                   (distance (cadr (graph-search (list (list nil 0 x)) (lambda (state) (equal (caddr state) y)) #'24-successors #'prepend #'24-state=))))
               (setf (gethash xy ht) distance
                     (gethash yx ht) distance)))
    ht))

(defun shortest-path (paths pairwise-path-lengths)
  (loop for path in paths
        minimize (loop for (x y) on path
                       when y
                         sum (gethash (cons x y) pairwise-path-lengths))))

(defun shortest-tsp-path (paths pairwise-path-lengths)
  (loop for path in paths
        minimize (loop for (x y) on path
                       if y
                         sum (gethash (cons x y) pairwise-path-lengths)
                       else
                         sum (gethash (cons x #\0) pairwise-path-lengths))))

(defun day-24-part-1 (input-file)
  (multiple-value-bind (*grid* starts)
      (read-grid input-file :starts? #'digit-char-p)
    (shortest-path (generate-possible-paths (points-of-interest))
                   (pairwise-shortest-path-lengths starts))))

(defun day-24-part-2 (input-file)
  (multiple-value-bind (*grid* starts)
      (read-grid input-file :starts? #'digit-char-p)
    (shortest-tsp-path (generate-possible-paths (points-of-interest))
                       (pairwise-shortest-path-lengths starts))))

(defun day-24 ()
  (let ((f (fetch-day-input-file 2016 24)))
    (values (day-24-part-1 f)
            (day-24-part-2 f))))
