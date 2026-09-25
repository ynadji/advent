(in-package :aoc2016)

(defun simplify-rooms (s)
  "Remove the 'doors' and force only cardinal directions so you can re-use your grid/search primitives."
  (let ((s (loop for line in (uiop:split-string s :separator '(#\Newline))
                 unless (str:starts-with? "#-#" line)
                   collect (if (str:starts-with? "##" line)
                               (remove #\# line :count 3)
                               (remove #\| line)))))
    (str:join #\Newline s)))

(defparameter rooms (parse-grid (simplify-rooms "#########
#S| | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | |  
####### V")))

(let ((mapping '((:north . #\U) (:south . #\D) (:west . #\L) (:east . #\R))))
  (defun swap-dirs (x)
    (etypecase x
      (character (car (rassoc x mapping)))
      (keyword (cdr (assoc x mapping))))))

(defun hash->wanted-directions (s)
  (let ((hash-chars (apply #'concatenate 'string (map 'list (lambda (n) (format nil "~2,'0x" n))
                                                      (subseq (md5:md5sum-string s) 0 2)))))
    (loop for c across hash-chars for direction in '(:north :south :west :east)
          when (find c "BCDEF")
            collect direction)))

(defun hash->wanted-directions/deltas (s)
  (apply #'directions->deltas (hash->wanted-directions s)))

(defun 17-reachable? (m pos dir)
  (declare (ignore dir))
  (find (paref m pos) "S V"))

(defun 17-successors (state)
  (let ((s (car state))
        (pos (cdr state)))
    (multiple-value-bind (positions directions)
        (2d-neighbors rooms pos :reachable? #'17-reachable? :wanted-directions (hash->wanted-directions/deltas s))
      (loop for pos in positions for dir in directions
            collect (cons (concatenate 'string s (list (swap-dirs dir))) pos)))))

(defun 17-goal? (state)
  (equal (cdr state) '(4 . 4)))

;; you could just do one GRAPH-SEARCH-ALL and pick the first/last, but part 1 takes < 1ms.
(defun day-17-part-1 (&optional (passcode "yjjvjgan"))
  (let ((goal-state (graph-search (list (cons passcode '(1 . 1))) #'17-goal? #'17-successors #'prepend #'equal)))
    (str:replace-all passcode "" (car goal-state))))

(defun day-17-part-2 (&optional (passcode "yjjvjgan"))
  (let ((goal-states (graph-search-all (list (cons passcode '(1 . 1))) #'17-goal? #'17-successors #'prepend #'equal)))
    (length (str:replace-all passcode "" (caar goal-states)))))

(defun day-17 ()
  (values (day-17-part-1)
          (day-17-part-2)))
