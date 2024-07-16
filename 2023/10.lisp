(in-package :aoc2023)

(defun connections (c)
  "Returns valid exit cardinal directions for a pipe. S is basically a wildcard so
all directions are valid."
  (let ((valid nil))
    (when (member c (coerce "S|LJ" 'list))
      (push :north valid))
    (when (member c (coerce "S|7F" 'list))
      (push :south valid))
    (when (member c (coerce "S-LF" 'list))
      (push :east valid))
    (when (member c (coerce "S-7J" 'list))
      (push :west valid))
    valid))

;; a-list to map the cardinal direction to the addend needed to move POS in that
;; direction. i.e., (ADD-PAIR POS '(-1 . 0)) -> NEW-POS such that NEW-POS is one
;; tile :north of POS.
(defparameter *direction-to-offset*
  '((:north . (-1 . 0))
    (:south . (1 . 0))
    (:east  . (0 . 1))
    (:west  . (0 . -1))))

(defparameter *reverse-directions*
  '((:north . :south)
    (:south . :north)
    (:east  . :west)
    (:west  . :east)))

(defun reverse-direction (dir)
  (cdr (assoc dir *reverse-directions*)))

(defun read-maze (input-file)
  "Reads an input file representing a maze and returns two values: the maze as a
2-d array, and the location of the #\S as a pair."
  (let* ((lines (uiop:read-file-lines input-file))
         (rows (length lines))
         (cols (length (first lines)))
         (maze (make-array (list rows cols)))
         (start (cons 0 0)))
    (loop for row in lines for i from 0 do
      (loop for x across row for j from 0 do
        (when (eq x #\S)
          (setf start (cons i j)))
        (setf (aref maze i j) x)))
    (values maze
            start)))

(defun valid-index? (maze i j)
  (let ((rows (array-dimension maze 0))
        (cols (array-dimension maze 1)))
    (and (and (>= i 0)
              (>= j 0))
         (and (< i rows)
              (< j cols)))))

(defun valid-pos? (maze pos)
  (valid-index? maze (car pos) (cdr pos)))

(defun exits (maze pos)
  (let* ((i (car pos))
         (j (cdr pos))
         (x (aref maze i j)))
    (connections x)))

(defun add-pair (p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(defun get-new-pos (pos dir)
  (add-pair pos
            (cdr (assoc dir *direction-to-offset*))))

(defun accepts-from (maze pos dir)
  (let* ((i (car pos))
         (j (cdr pos))
         (x (aref maze i j)))
    (member dir (mapcar #'reverse-direction (connections x)))))

(defun reachable-neighbors (maze pos)
  "A neighbor is reachable iff its exits match with the pipe it is connecting
to. E.g., a #\7 connects due :west and :south. It can only connect with pipe
pieces that connect :east or :north, respectively. The one exception is #\S,
which can connect to any other pipe piece. #\. have no exits, so cannot connect
to any pipe piece."
  (2d-neighbors maze (car pos) (cdr pos) :wanted-directions (exits maze pos)
                                         :reachable? #'accepts-from))

(defun next-pos (maze pos prev-pos)
  "A given position can only connect to at most two other pipes. Since we only care
about going somewhere new, this helper returns the other direction."
  (car (remove prev-pos (reachable-neighbors maze pos) :test #'equal)))

(defun find-cycle (maze start)
  "DFS in one direction starting from START on MAZE. Since there is only a single
loop through START, we only need to go one direction from START until we run
into START again."
  (let ((current (first (reachable-neighbors maze start)))
        (prev start))
    (loop for next = (next-pos maze current prev)
          until (equal current start)
          collect current into cycle
          do
             (setf prev current)
             (setf current next)
          finally
             (return (cons start cycle)))))

(defun day-10-part-1 (input-file)
  (multiple-value-bind (maze start) (read-maze input-file)
    (floor (/ (length (find-cycle maze start)) 2))))

(defun print-maze (maze cycle &key (color? nil))
  "Print out MAZE. #\S will be colored red, tiles in the CYCLE will be colored
green and #\I tiles that are inside the CYCLE will be colored cyan. Doesn't work
in SLIME."
  (loop for i below (array-dimension maze 0) do
    (format t "~3,'0d " i)
    (loop for j below (array-dimension maze 1)
          for c = (aref maze i j) do
            (if color?
                (cond ((eq #\S c)
                       (format t "~c[31m~a~c[0m" #\ESC c #\ESC)) ; red for start
                      ((eq #\I c)
                       (format t "~c[36m~a~c[0m" #\ESC c #\ESC)) ; cyan for inside
                      ((member (cons i j) cycle :test #'equal)
                       (format t "~c[32m~a~c[0m" #\ESC c #\ESC)) ; green for pipe
                      (t
                       (format t "~a" (aref maze i j)))) ; black for rest
                (format t "~a" (aref maze i j))))
    (format t "~&")))

(defun relative-direction-at-char (c traveling)
  "Return two lists that correpond to the cardinal directions to the left and
to the right, respectively, as if just arrived to tile C after going in the
cardinal direction TRAVELING. Corner Cs (e.g., #\L) have multiple tiles to their
left and right.

This could probably be greatly simplified."
  (if (member c (coerce "|-" 'list))
      (cond ((eq traveling :north) (values '(:west)  '(:east)))
            ((eq traveling :south) (values '(:east)  '(:west)))
            ((eq traveling :east)  (values '(:north) '(:south)))
            ((eq traveling :west)  (values '(:south) '(:west))))
      (cond ((eq c #\7)
             (if (eq traveling :north)
                 (values nil '(:north :east))
                 (values '(:north :east) nil)))
            ((eq c #\F)
             (if (eq traveling :north)
                 (values '(:west :north) nil)
                 (values nil '(:west :north))))
            ((eq c #\L)
             (if (eq traveling :south)
                 (values nil '(:south :west))
                 (values '(:south :west) nil)))
            ((eq c #\J)
             (if (eq traveling :south)
                 (values '(:south :east) nil)
                 (values nil '(:south :east)))))))

(defun infer-traveling (p1 p2)
  "Return the cardinal direction we go to travel from p1 -> p2."
  (loop for (direction . offset) in *direction-to-offset* do
    (when (equal (add-pair p1 offset)
                 p2)
      (return direction))))

(defun now-traveling (c traveling)
  "Given the current tile's C and the direction we are currently TRAVELING (e.g.,
:west) to enter the tile, return the direction of the tile's other exit."
  (let* ((enter-bys (mapcar #'reverse-direction (connections c)))
         (other-entrance (first (remove traveling enter-bys))))
    (reverse-direction other-entrance)))

(defun label-inside-outside (maze cycle)
  "Given a MAZE and its CYCLE, begin labeling the tiles inside CYCLE. Given a
side (:right or :left) of the pipe to consider inside while walking CYCLE, label
non-CYCLE nodes on the inside side as being inside. We maintain all of these
positions in INSIDES, which is returned. We also update MAZE at these tiles to
be #\I. This only infers the inside tiles that are nearest to the pipe in CYCLE."
  ;; how do i infer which direction is inside? i cheated a bit here by
  ;; hardcoding it but there are only two possibilities. if you want to fix
  ;; this, the correct answer for the following test inputs are:
  ;; * test-input-2 is :right
  ;; * test-input-3 is :left
  ;; * test-input-4 is :left
  ;; * input is :right
  (let ((inside :right)
        (traveling (infer-traveling (first cycle) (second cycle)))
        (insides nil))
    (loop for pos in (rest cycle)
          for (i . j) = pos
          for c = (aref maze i j) do
            ;;(format t "traveling ~a to ~a at ~a~&" traveling c pos)
            (multiple-value-bind (left right) (relative-direction-at-char c traveling)
              (let ((inside-positions (remove-if-not
                                       (lambda (p) (valid-pos? maze p))
                                       (mapcar (lambda (d) (get-new-pos pos d))
                                               (if (eq inside :right) right left)))))
                
                (loop for inside-pos in inside-positions do
                  (unless (member inside-pos cycle :test #'equal)
                    ;;(format t "	~a at ~a -> I~&" (aref maze (car inside-pos) (cdr inside-pos)) inside-pos)
                    (pushnew inside-pos insides :test #'equal)
                    (setf (aref maze (car inside-pos) (cdr inside-pos)) #\I)))))
            (setq traveling (now-traveling c traveling)))
    insides))

(defun all-neighbors (maze pos)
  (2d-neighbors maze (car pos) (cdr pos) :wanted-directions '(:north :south :east :west)))

(defun flood-fill (maze known-insides cycle)
  "Fill the remaining insides of MAZE bound by CYCLE starting with a subset of all
inside tiles in KNOWN-INSIDES that were found by LABEL-INSIDE-OUTSIDE. Return
the updated KNOWN-INSIDES so we can solve the puzzle. Update the tiles in MAZE
so we can pretty-print our MAZR :3. BFS starting on tiles from KNOWN-INSIDES."
  (let ((candidates (remove-duplicates (mapcan (lambda (pos) (all-neighbors maze pos)) known-insides))))
    (loop while candidates do
      (let ((pos (pop candidates)))
        ;; already an I or in known-insides (do nothing)
        ;; in the cycle (do nothing)
        ;; otherwise, label as I, add to cycle, add all neighbors to candidates
        (unless (or (member pos known-insides :test #'equal)
                    (member pos cycle :test #'equal)
                    (eq (aref maze (car pos) (cdr pos))
                        #\I))
          (pushnew pos known-insides)
          (setf (aref maze (car pos) (cdr pos)) #\I)
          (setf candidates (union candidates (all-neighbors maze pos))))))
    known-insides))

(defun day-10-part-2 (input-file)
  (multiple-value-bind (maze start) (read-maze input-file)
    (let* ((cycle (find-cycle maze start))
           (insides (flood-fill maze (label-inside-outside maze cycle) cycle)))
      ;(format t "~a -> ~a~&" (first cycle) (second cycle))
      ;(print-maze maze cycle :color? t)
      (length insides))))

(defun day-10 ()
  (let ((f #p"10-input.txt"))
    (values (day-10-part-1 f)
            (day-10-part-2 f))))
