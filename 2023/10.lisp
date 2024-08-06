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
         (maze (make-array (list rows cols) :element-type 'base-char))
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
  (2d-neighbors maze pos :wanted-directions (exits maze pos)
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
                       ;; \e[0;30m
                       (format t "~c[30m~a~c[0m" #\ESC c #\ESC))) ; black for rest
                (format t "~a" (aref maze i j))))
    (format t "~&")))

(defun determinant (pos1 pos2)
  (- (* (car pos1) (cdr pos2))
     (* (cdr pos1) (car pos2))))

;; gets 2A
(defun shoelace (positions)
  (abs (loop for (pos1 pos2) on (append positions (-> positions first list)) by #'cdr
             when pos2
               sum (determinant pos1 pos2))))

;; A = i + b/2 - 1
;; i is answer
;; b is length of cycle
;; A - b/2 + 1 = i
;;
;; shoelace/2 - cycle-length/2 + 1 = i
(defun picks-theorem (shoelace cycle-length)
  (1+ (- (/ shoelace 2)
         (/ cycle-length 2))))

(defun day-10-part-2 (input-file)
  (multiple-value-bind (maze start) (read-maze input-file)
    (let ((cycle (find-cycle maze start)))
      ;;(format t "~a -> ~a~&" (first cycle) (second cycle))
      ;;(print-maze maze cycle :color? t)
      (picks-theorem (shoelace cycle) (length cycle)))))

(defun day-10 ()
  (let ((f #p"10-input.txt"))
    (values (day-10-part-1 f)
            (day-10-part-2 f))))
