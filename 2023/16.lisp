(in-package :aoc2023)

(defun light-reachable? (M pos dir)
  (let ((c (aref M (car pos) (cdr pos)))
        (in-dir (opposite-direction dir)))
    (cond ((char= c #\.) t)
          (t in-dir))))

(defun next-directions (c dir)
  (case c
    (#\. (list dir))
    (#\| (if (or (eq dir :west) (eq dir :east))
             (list :north :south)
             (list dir)))
    (#\- (if (or (eq dir :north) (eq dir :south))
             (list :east :west)
             (list dir)))
    (#\/ (case dir (:north '(:east)) (:east '(:north)) (:south '(:west)) (:west '(:south))))
    (#\\ (case dir (:north '(:west)) (:west '(:north)) (:south '(:east)) (:east '(:south))))))

(defun visit-equal (pd1 pd2)
  (and (equal (car pd1) (car pd2))
       (or (eq (cdr pd1) (cdr pd2))
           (eq (cdr pd1) (opposite-direction (cdr pd2))))))

(defun pew-pew (grid start direction)
  ;; visited needs to track both the location _and_ the direction it
  ;; entered with so we can make sure perpendicular crossings continue. i
  ;; _think_ that's the only case when things should be revisited, which
  ;; means :east/:west and :north/:south should both count, i.e., if we
  ;; entered from going :north if we go :south through the same point we
  ;; will only repeat. and likewise for :east/:west.
  ;;
  ;; well that's def not true!
  (let (to-visit directions visited)
    (dolist (nd (next-directions (aref grid (car start) (cdr start)) direction))
      (push start to-visit)
      (push nd directions))
    (loop while (and to-visit directions) do
      (let* ((curr-pos (pop to-visit))
             (dir (pop directions))
             (pos-dir (cons curr-pos dir)))
        ;;(format t "(when (not (member ~a ~a :test #'equal)))~%" pos-dir visited)
        (when (not (member pos-dir visited :test #'equal))
          ;;(format t "visiting ~a from ~a~%" curr-pos dir)
          (push pos-dir visited)
          (multiple-value-bind (next-positions next-directions) (2d-neighbors grid (car curr-pos) (cdr curr-pos) :wanted-directions (list dir))
            ;;(format t "next-positions: ~a next-directions: ~a~%" next-positions next-directions)
            (loop for np in next-positions for nds in next-directions
                  ;; NEXT-DIRECTIONS can return multiple values, e.g., from
                  ;; #\- and #\| splitters.
                  do ;;(format t "np: ~a, nds: ~a~%" np nds)
                     ;;(format t "c: ~@c next-directions: ~a~%" (aref grid (car np) (cdr np)) (next-directions (aref grid (car np) (cdr np)) nds))
                     (dolist (nd (next-directions (aref grid (car np) (cdr np)) nds))
                       (push np to-visit)
                       (push nd directions)
                       ;;(format t "to-visit: ~a, directions: ~a~%" to-visit directions)
                       ))))))
    visited))

(defun day-16-part-1 (input-file)
  (let* ((maze (read-maze input-file))
         (visited-dirs (pew-pew maze '(0 . 0) :east))
         (visited (remove-duplicates (mapcar #'car visited-dirs) :test #'equal)))
    ;(print-maze maze visited :color? t)
    (length visited)
    ))

(defun day-16-part-2 (input-file) (progn input-file -1))

(defun day-16 ()
  (let ((f (fetch-day-input-file 2023 16)))
    (values (day-16-part-1 f)
            (day-16-part-2 f))))
