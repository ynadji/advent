(in-package :aoc2023)

(defun read-heat-map (input-file)
  (let* ((lines (uiop:read-file-lines input-file))
         (rows (length lines))
         (cols (length (first lines)))
         (map (make-array (list rows cols) :element-type 'fixnum)))
    (loop for row in lines for i from 0 do
      (loop for x across row for j from 0 do
        (setf (aref map i j) (digit-char-p x))))
    map))

;; i need to encode the 3 in same direction thing.  absolute difference of i and
;; j? ah can also turn left or right grrr.
;;
;; so yeah, 2d-neighbors, reachable does the diff, and possible directions are
;; everything but backwards. still try to do the recursive bit such that you
;; don't need to cache HEAT-MAP on every call.
(defun min-cost-path-rec (heat-map)
  (labels ((aux (i j)
             (if (= i j 12)
                 (aref heat-map i j)
                 (+ (cond ((= 3 (- i j)) (aux i (1+ j)))
                          ((= 3 (- j i)) (aux (1+ i) j))
                          ((and (< i 12) (< j 12))
                           (min (aux (1+ i) j)
                                (aux i (1+ j))))
                          ((< i 12) (aux (1+ i) j))
                          ((< j 12) (aux i (1+ j))))
                    (aref heat-map i j)))))
    (aux 0 0)))

(defun day-17-part-1 (input-file) (progn input-file -1))

(defun day-17-part-2 (input-file) (progn input-file -1))

(defun day-17 ()
  (let ((f (fetch-day-input-file 2023 17)))
    (values (day-17-part-1 f)
            (day-17-part-2 f))))
