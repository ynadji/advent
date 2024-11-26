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

(defstruct heat-walk-meta (pos (cons 0 0)) direction (steps 0))

(defun min-heat-loss (heat-map start end)
  (let* ((end (cons (array-dimension heat-map 0)
                    (array-dimension heat-map 1)))
         (distances (make-hash-table :test #'equalp))
         (q (queues:make-queue
             :priority-queue
             :compare (lambda (x y)
                        (< (gethash x distances most-positive-fixnum)
                           (gethash y distances most-positive-fixnum))))))
    ;;(queues:) ; just stopped mid thought here.
    (loop with curr = (pop to-visit)
          until (equal curr end)
          do (multiple-value-bind (new-ps dirs) (2d-neighbors heat-map curr)
               (loop for new-p in new-ps for d in dirs)))))

(defun day-17-part-1 (input-file) (progn input-file -1))

(defun day-17-part-2 (input-file) (progn input-file -1))

(defun day-17 ()
  (let ((f (fetch-day-input-file 2023 17)))
    (values (day-17-part-1 f)
            (day-17-part-2 f))))

;; from https://github.com/ak-coram/advent2023/blob/main/17.lisp
#+(and)
(defun day17-stolen (input-file is-part-two)
  (let* ((lines (uiop:read-file-lines input-file))
         (width (length (car lines))) (height (length lines))
         (min-steps (if is-part-two 4 0))
         (max-steps (if is-part-two 10 3)))
    (labels ((gethash-infinity (key ht) (gethash key ht most-positive-fixnum))
             (neighbours (pos)
               (destructuring-bind (x y d steps) pos
                 (loop for (x y direction opposite-direction)
                         in `((,(1- x) ,y :left :right)
                              (,(1+ x) ,y :right :left)
                              (,x ,(1- y) :up :down)
                              (,x ,(1+ y) :down :up))
                       for is-same-direction = (eql d direction)
                       when (and (not (eql d opposite-direction))
                                 (< -1 x width) (< -1 y height)
                                 (if (or (null d) is-same-direction)
                                     (< steps max-steps)
                                     (<= min-steps steps)))
                         collect (list x y direction (if is-same-direction
                                                         (1+ steps)
                                                         1)))))
             (weight (pos) (digit-char-p (char (nth (cadr pos) lines)
                                               (car pos))))
             (min-heat-loss (start goal)
               (let* ((from (append start (list nil 0)))
                      (dist (make-hash-table :test 'equal))
                      (prev (make-hash-table :test 'equal))
                      (q (queues:make-queue
                          :priority-queue
                          :compare (lambda (a b)
                                     (< (gethash-infinity a dist)
                                        (gethash-infinity b dist))))))
                 (setf (gethash from dist) 0)
                 (queues:qpush q from)
                 (loop for u = (queues:qpop q) while u
                       do (loop for n in (neighbours u)
                                for alt := (+ (gethash-infinity u dist)
                                              (weight n))
                                when (< alt (gethash-infinity n dist))
                                  do (progn (setf (gethash n dist) alt
                                                  (gethash n prev) u)
                                            (queues:qpush q n))))
                 (loop for k being the hash-keys of prev
                       for (x y nil nil) = k
                       when (equal (list x y) goal)
                         minimize (gethash k dist)))))
      (min-heat-loss '(0 0) (list (1- width) (1- height))))))
