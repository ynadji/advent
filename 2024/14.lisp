(in-package :aoc2024)

(defparameter test-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defun compute-robot-location (position-and-velocity max-x max-y n)
  (declare (optimize (speed 3) (safety 0)))
  (destructuring-bind (x y dx dy) position-and-velocity
    (declare (type fixnum x y dx dy max-x max-y n))
    (let ((dxn (the fixnum (* dx n)))
          (dyn (the fixnum (* dy n))))
      (cons (mod (the fixnum (+ x dxn)) max-x)
            (mod (the fixnum (+ y dyn)) max-y)))))

(defun read-robots (input-file)
  (mapcar #'string-to-num-list (uiop:read-file-lines input-file)))

(defun count-robot-quadrants (robots max-x max-y)
  (let ((x-barrier (floor (/ max-x 2)))
        (y-barrier (floor (/ max-y 2))))
    (* (length (remove-if-not (lambda (p) (and (< (car p) x-barrier)
                                          (< (cdr p) y-barrier)))
                              robots))
       (length (remove-if-not (lambda (p) (and (> (car p) x-barrier)
                                          (< (cdr p) y-barrier)))
                              robots))
       (length (remove-if-not (lambda (p) (and (< (car p) x-barrier)
                                          (> (cdr p) y-barrier)))
                              robots))
       (length (remove-if-not (lambda (p) (and (> (car p) x-barrier)
                                          (> (cdr p) y-barrier)))
                              robots)))))

(defun day-14-part-1 (input-file)
  (let ((max-x 101)
        (max-y 103))
   (count-robot-quadrants (mapcar (lambda (pv)
                                    (compute-robot-location pv max-x max-y 100))
                                  (read-robots input-file)) max-x max-y)))

(defun day-14-part-2 (input-file)
  (let* ((robots (read-robots input-file))
         (max-x 101)
         (max-y 103)
         (num-robots (length robots)))
    (loop for n from 1 for new-robots = (mapcar (lambda (pv) (compute-robot-location pv max-x max-y n)) robots)
          when (= num-robots (length (remove-duplicates new-robots :test #'equal)))
            return n)))

(defun draw-robots (input-file n)
  (let* ((robots (read-robots input-file))
         (max-x 101)
         (max-y 103)
         (grid (make-array (list max-y max-x) :initial-element 0))
         (positions (mapcar (lambda (pv) (compute-robot-location pv max-x max-y n)) robots)))
    (loop for pos in positions do (incf (aref grid (cdr pos) (car pos))))
    (with-open-file (stream #P"14-xmas-tree.txt" :direction :output :if-exists :supersede)
     (print-grid grid :stream stream :char-color-alist '((0 . :black) (1 . :green))))))

(defun day-14 ()
  (let ((f (fetch-day-input-file 2024 14)))
    (values (day-14-part-1 f)
            (day-14-part-2 f))))
