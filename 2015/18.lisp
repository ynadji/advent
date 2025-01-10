(in-package :aoc2015)

(defparameter test-input ".#.#.#
...##.
#....#
..#...
#.#..#
####..")

(defun do-step (grid-old grid-new &optional part2)
  (labels ((neighbors (grid i j)
             (mapcar (lambda (pos) (aref grid (car pos) (cdr pos)))
                     (2d-neighbors grid (cons i j) :wanted-directions *8-winds/deltas*)))
           (change? (grid i j)
             (let* ((neighbors (neighbors grid i j))
                    (num-on-neighbors (count #\# neighbors)))
               (if (and part2 (= 3 (length neighbors)))
                   #\#
                   (ecase (aref grid i j)
                     (#\. (when (= 3 num-on-neighbors)
                            #\#))
                     (#\# (unless (member num-on-neighbors '(2 3))
                            #\.)))))))
    (aops:each-index (i j)
      (ax:if-let ((on/off (change? grid-old i j)))
        (setf (aref grid-new i j) on/off)
        (setf (aref grid-new i j) (aref grid-old i j))))))

(defun turn-corners-on (grid)
  (destructuring-bind (max-i max-j) (mapcar #'1- (array-dimensions grid))
    (setf (aref grid 0 0) #\#
          (aref grid max-i 0) #\#
          (aref grid 0 max-j) #\#
          (aref grid max-i max-j) #\#))
  grid)

(defun day-18% (input-file &optional part2 (n 100))
  (loop repeat n
        with grid-old = (if part2
                            (turn-corners-on (read-grid input-file))
                            (read-grid input-file))
        with grid-new = (if part2
                            (turn-corners-on (read-grid input-file))
                            (read-grid input-file))
        do (do-step grid-old grid-new part2)
           (psetf grid-old grid-new
                  grid-new grid-old)
        finally (return (aops:sum-index (i j)
                          (if (char= #\# (aref grid-old i j)) 1 0)))))

(defun day-18 ()
  (let ((f (fetch-day-input-file 2015 18)))
    (values (day-18% f)
            (day-18% f t))))
