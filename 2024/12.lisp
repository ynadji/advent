(in-package :aoc2024)

(defparameter test-input-1 "AAAA
BBCD
BBCC
EEEC")

(defparameter test-input-2 "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(defstruct region points char area perimeter)

(defun adjacent? (p1 p2)
  (or (and (= (car p1) (car p2))
           (= 1 (abs (- (cdr p1) (cdr p2)))))
      (and (= (cdr p1) (cdr p2))
           (= 1 (abs (- (car p1) (car p2)))))))

(defun find-region (grid start)
  (let ((c (aref grid (car start) (cdr start)))
        (states (list start))
        visited
        (perimeter 0))
    (loop while states for state = (pop states)
          for neighbors = (2d-neighbors grid state :reachable? (lambda (grid pos dir)
                                                                 (declare (ignore dir))
                                                                 (char= (aref grid (car pos) (cdr pos)) c)))
          do (incf perimeter (- 4 (length neighbors)))
          do (push state visited)
             (loop for neighbor in neighbors when (not (member neighbor visited :test #'equal))
                   do (pushnew neighbor states :test #'equal)))
    (make-region :points visited :char c :area (length visited) :perimeter perimeter)))

(defun find-regions (grid)
  (let (regions visited)
    (loop for i from 0 below (array-dimension grid 0) do
          (loop for j from 0 below (array-dimension grid 1)
                for c = (aref grid i j)
                unless (member (cons i j) visited :test #'equal)
                  do (push (find-region grid (cons i j)) regions)
                     (setf visited (append (region-points (first regions)) visited))))
    regions))

(defun day-12-part-1 (input-file)
  (let ((grid (read-grid input-file)))
   (loop for region in (find-regions grid)
         sum (* (region-area region) (region-perimeter region)))))

(defun day-12-part-2 (input-file) (progn input-file -1))

(defun day-12 ()
  (let ((f (fetch-day-input-file 2024 12)))
    (values (day-12-part-1 f)
            (day-12-part-2 f))))
