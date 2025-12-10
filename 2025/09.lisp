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

(declaim (inline line-doesnt-intersect-rectangle?))
(defun line-doesnt-intersect-rectangle? (line p1 p2)
  (declare (optimize speed))
  (let* ((lx (the fixnum (first (first line))))
         (ly (the fixnum (second (first line))))
         (lx-prime (the fixnum (first (second line))))
         (ly-prime (the fixnum (second (second line))))
         (x (the fixnum (first p1)))
         (y (the fixnum (second p1)))
         (x-prime (the fixnum (first p2)))
         (y-prime (the fixnum (second p2))))
    (or (<= (max lx lx-prime) (min x x-prime))
        (>= (min lx lx-prime) (max x x-prime))
        (<= (max ly ly-prime) (min y y-prime))
        (>= (min ly ly-prime) (max y y-prime)))))

(defun intersects? (p1 p2 polygon)
  (let ((length (array-dimension polygon 0)))
    (loop for i below length
          for p3 = (aref polygon i)
          for p4 = (aref polygon (mod (1+ i) length))
            thereis (not (line-doesnt-intersect-rectangle? (list p3 p4) p1 p2)))))

(defun day-09-part-2 (input-file)
  (let ((polygon (read-red-squares input-file)))
    (loop for i from 0 below (array-dimension polygon 0)
          maximize (loop for j from (1+ i) below (array-dimension polygon 0)
                         when (not (intersects? (aref polygon i) (aref polygon j) polygon))
                           maximize (rectangle-area (aref polygon i) (aref polygon j))))))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2025 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
