(in-package :aoc2016)

(defparameter test-input "###########
#0.1.....2#
#.#######.#
#4.......3#
###########")

(defun 24-reachable? (m pos dir)
  (declare (ignore dir))
  (char/= (paref m pos) #\#))

(defun plusp-poi? (c)
  (and (digit-char-p c)
       (char/= c #\0)))

(defun points-of-interest (grid)
  (remove-if-not #'digit-char-p (map 'list #'identity grid)))

(defun 24-state= (s1 s2)
  (and (equal (car s1) (car s2))
       (equal (caddr s1) (caddr s2))))

(sb-ext:define-hash-table-test 24-state= (lambda (s) (sxhash `(,(car s) (caddr s)))))

(defun generate-possible-paths (digits)
  (remove-if-not (equals #\0) (permutations digits) :key #'first))

(defun pairwise-shortest-path-lengths (grid number-positions)
  (flet ((24-successors (state)
           (multiple-value-bind (positions directions)
               (2d-neighbors grid (caddr state) :reachable? #'24-reachable?)
             (declare (ignorable directions))
             (loop with seen-numbers = (car state)
                   with new-steps = (1+ (cadr state))
                   for pos in positions for c = (paref grid pos)
                   collect (list (if (plusp-poi? c) (adjoin c seen-numbers) seen-numbers) new-steps pos)))))
    (let ((ht (make-hash-table :test #'equal)))
      (loop for (x y) in (combinations number-positions 2)
            do (let ((xy (cons (paref grid x) (paref grid y)))
                     (yx (cons (paref grid y) (paref grid x)))
                     (distance (cadr (graph-search (list (list nil 0 x)) (lambda (state) (equal (caddr state) y)) #'24-successors #'prepend #'24-state=))))
                 (setf (gethash xy ht) distance
                       (gethash yx ht) distance)))
      ht)))

(defun shortest-path (paths pairwise-path-lengths)
  (loop for path in paths
        minimize (loop for (x y) on path
                       when y
                         sum (gethash (cons x y) pairwise-path-lengths))))

(defun day-24% (input-file part)
  (let ((tsp (if (= part 1) #'identity (lambda (l) (append l '(#\0))))))
    (multiple-value-bind (grid starts)
        (read-grid input-file :starts? #'digit-char-p)
      (shortest-path (mapcar tsp (generate-possible-paths (points-of-interest grid)))
                     (pairwise-shortest-path-lengths grid starts)))))

(defun day-24 ()
  (let ((f (fetch-day-input-file 2016 24)))
    (values (day-24% f 1)
            (day-24% f 2))))
