(in-package :aoc2023)

(defun connections (c)
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
  (let* ((directions (exits maze pos))
         (positions (mapcar (lambda (dir) (get-new-pos pos dir)) directions)))
    (loop for direction in directions
          for position in positions
          when (and (valid-pos? maze position)
                    (accepts-from maze position direction))
            collect position)))

(defun next-pos (maze pos prev-pos)
  (car (remove prev-pos (reachable-neighbors maze pos) :test #'equal)))

(defun find-cycle (maze start)
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
  (loop for i below (array-dimension maze 0) do
    (format t "~3,'0d " i)
    (loop for j below (array-dimension maze 1) do
      (if (member (cons i j) cycle :test #'equal)
          (if color?
              (format t "~c[31m~a~c[0m" #\ESC (aref maze i j) #\ESC)
              (format t "~a" (aref maze i j)))
          (format t "~a" (aref maze i j))))
    (format t "~&")))

(defun day-10 ()
  (let ((f #p"10-input.txt"))
    (multiple-value-bind (maze start) (read-maze f)
      (let ((cycle (find-cycle maze start)))
        (print-maze maze cycle :color? t)))))
