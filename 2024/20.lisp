(in-package :aoc2024)

(defparameter test-input "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(declaim (optimize (speed 3)))

(defun add-all-dirs (pos)
  (loop for dir in *cardinals* collect (cons dir pos)))

(defun day-20-part-1 (input-file &optional (d 2) print?)
  (declare (type fixnum d))
  (multiple-value-bind (maze starts-and-ends) (read-grid input-file :starts? (lambda (c) (member c '(#\S #\E))))
    (multiple-value-bind (starts ends)
        (if (char= #\S (paref maze (first starts-and-ends)))
            (values (add-all-dirs (first starts-and-ends)) (add-all-dirs (second starts-and-ends)))
            (values (add-all-dirs (second starts-and-ends)) (add-all-dirs (first starts-and-ends))))
      (when print?
        (format t "starts: ~a~%ends: ~a~%" starts ends))
      (multiple-value-bind (dist prev) (dijkstra2 starts maze)
        (multiple-value-bind (min-state min-score)
            (min-score-state ends dist)
          (declare (ignore min-score))
          (let ((path (cons (cdr (first starts)) (reverse (remove-duplicates (mapcar #'cdr (walk-back prev min-state (cdar starts))) :test #'equal)))))
            (when print?
              (with-open-file (stream #P"20.txt" :direction :output :if-exists :supersede)
                (print-grid maze :stream stream :pos-color-alist (mapcar (lambda (pos) (cons pos :red)) (cons (cdr (first starts)) path)))))
            ;; count total-length
            ;; look at all pair-wise points that differ horizontally or vertically by 2
            ;; track indices of above
            ;; output (- total-length (abs (- i j)))
            (when print?
              (print path))
            (let ((total-length (length path)))
              (when print? (print total-length))
              (loop for i fixnum from 0 for x in path
                    sum
                    (loop for j fixnum from 0 for y in path
                          when (and (< i j) (<= (the fixnum (manhattan-distance x y)) d))
                            count (>= (- (the fixnum (abs (the fixnum (- i j)))) (the fixnum (manhattan-distance x y))) 100)
                          ;;collect (- (abs (- i j)) 2)
                          )))))))))

(defun day-20-part-2 (input-file)
  (day-20-part-1 input-file 20))

(defun day-20 ()
  (let ((f (fetch-day-input-file 2024 20)))
    (values (day-20-part-1 f)
            (day-20-part-2 f))))
