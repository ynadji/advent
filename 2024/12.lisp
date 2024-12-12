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

(defparameter test-input-3 "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE")

(defparameter test-input-4 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA")

(defstruct region points char area perimeter num-corners)

(defun adjacent? (p1 p2)
  (or (and (= (car p1) (car p2))
           (= 1 (abs (- (cdr p1) (cdr p2)))))
      (and (= (cdr p1) (cdr p2))
           (= 1 (abs (- (car p1) (car p2)))))))

(defun same-plane? (&rest points)
  (or (apply #'= (mapcar #'car points))
      (apply #'= (mapcar #'cdr points))))

(defun get-inter-cardinals (directions)
  (remove nil
          (remove-duplicates
           (loop for x in directions
                 append (loop for y in directions
                              collect (make-inter-cardinal x y))))))

(defun count-corners (grid p neighbors directions reachable?)
  (ecase (length neighbors)
    ;; no neighbors, we're in the middle and don't have any corners.
    (0 4)
    ;; 1 neighbor is:
    ;;     .
    ;; 0000
    ;;     .
    ;; so we have two corners
    (1 2)
    ;; if we have 3 neighbors, the number of plants in the corners that are not
    ;; the same is the number of corners. think of a tetris T
    ;;
    ;; 000
    ;; .0.
    ;;
    ;; the .s could be corners iff they are not also 0s.
    (3 (length (2d-neighbors grid p :reachable? (ax:compose #'not reachable?)
                                    :wanted-directions (get-inter-cardinals directions))))
    ;; diagonals are all possible corners if they aren't the same plant. consider:
    ;;
    ;; .0.
    ;; 000
    ;; .0.
    (4 (- 4 (- (length (2d-neighbors grid p :reachable? reachable? :wanted-directions *8-winds*))
               (length neighbors))))
    (2 (cond ((apply #'same-plane? p neighbors) 0) ; middle in a line
             ((null (2d-neighbors grid p :reachable? reachable? :wanted-directions (list (apply #'make-inter-cardinal directions))))
              ;; if the corner created by my neighbors isn't mine, i have corners on both sides
              ;; think of:
              ;;
              ;; 0c
              ;; 00
              ;;c
              ;;
              ;; cs are corners.
              2)
             (t 1)))))

(defun find-region (grid start)
  (let ((c (aref grid (car start) (cdr start)))
        (states (list start))
        visited
        (perimeter 0)
        (num-corners 0))
    (labels ((same-plant? (grid pos dir)
               (declare (ignore dir))
               (char= (aref grid (car pos) (cdr pos)) c)))
      (loop while states for state = (pop states)
            for (neighbors directions) = (multiple-value-list (2d-neighbors grid state :reachable? #'same-plant?))
            do (incf perimeter (- 4 (length neighbors)))
            do (incf num-corners (count-corners grid state neighbors directions #'same-plant?))
            do (push state visited)
               (loop for neighbor in neighbors when (not (member neighbor visited :test #'equal))
                     do (pushnew neighbor states :test #'equal))))
    (make-region :points visited :char c :area (length visited) :perimeter perimeter :num-corners num-corners)))

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

(defun day-12-part-2 (input-file)
  (let ((grid (read-grid input-file)))
   (loop for region in (find-regions grid)
         sum (* (region-area region) (region-num-corners region)))))

(defun do-all-part-2-tests ()
  (loop for test-input in (list test-input-1 test-input-2 test-input-3 test-input-4)
        do (with-input (input-file test-input)
             (print (day-12-part-2 input-file)))))

(defun day-12 ()
  (let ((f (fetch-day-input-file 2024 12)))
    (values (day-12-part-1 f)
            (day-12-part-2 f))))
