(in-package :aoc2025)

(defparameter test-input "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defun read-red-squares (input-file)
  (coerce (loop for line in (uiop:read-file-lines input-file)
                collect (string-to-num-list line))
          'vector))

(defun rectangle-area (p1 p2)
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (* (1+ (abs (- x2 x1)))
         (1+ (abs (- y2 y1)))))))

(defun day-09-part-1 (input-file)
  (let ((points (read-red-squares input-file)))
    (loop for i from 0 below (array-dimension points 0)
          maximize (loop for j from (1+ i) below (array-dimension points 0)
                         maximize (rectangle-area (aref points i) (aref points j))))))

(defun rectangle-edges (p1 p2)
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (let ((min-x (min x1 x2))
            (min-y (min y1 y2))
            (max-x (max x1 x2))
            (max-y (max y1 y2)))
        (list `(((,min-x ,max-y) (,min-x ,min-y)) ; left
                ((,min-x ,min-y) (,max-x ,min-y)) ; top
                ((,max-x ,min-y) (,max-x ,max-y)) ; right
                ;; bottom
                ((,max-x ,max-y) (,min-x ,max-y))))))))

(defun intersects? (rectangle line)
  (destructuring-bind ((r-left r-top r-right r-bottom)) rectangle
    (destructuring-bind ((lx1 ly1) (lx2 ly2)) line
      (or (print (<= (max lx1 lx2) (min (first (first r-left)) (second (first r-left)))))
          (print (>= (min lx1 lx2) (max (first (first r-right)) (second (first r-right)))))
          (print (<= (max ly1 ly2) (min (first (second r-bottom)) (second (second r-bottom)))))
          (print (>= (min ly1 ly2) (max (first (second r-top)) (second (second r-top)))))))))

(defun contains-rectangle? (polygon rectangle)
  (let ((length (length polygon)))
    (loop for i below length
          for p1 = (aref polygon i)
          for p2 = (aref polygon (mod (1+ i) length))
          never (intersects? rectangle (list p1 p2)))))

(defun day-09-part-2 (input-file) (progn input-file -1))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2025 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
