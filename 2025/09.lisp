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
      (format t "~a ~a ~a ~a~%~%"
              (<= (max lx1 lx2) (min (first (first r-left)) (first (second r-left))))
              (>= (min lx1 lx2) (max (first (first r-right)) (first (second r-right))))

              (<= (max ly1 ly2) (min (second (first r-top)) (second (second r-top))))
              (>= (min ly1 ly2) (max (second (first r-bottom)) (second (second r-bottom)))))
      (or (<= (max lx1 lx2) (min (first (first r-left)) (first (second r-left))))
          (>= (min lx1 lx2) (max (first (first r-right)) (first (second r-right))))

          (<= (max ly1 ly2) (min (second (first r-top)) (second (second r-top))))
          (>= (min ly1 ly2) (max (second (first r-bottom)) (second (second r-bottom))))))))

(defun contains-rectangle? (polygon rectangle)
(let ((length (length polygon)))
  (loop for i from 0 below length
        for p1 = (aref polygon i)
        for p2 = (aref polygon (mod (1+ i) length))
        do (format t "~a ~a --- ~a~%" p1 p2 rectangle)
          thereis (intersects? rectangle (list p1 p2)))))

(defun day-09-part-2 (input-file)
  (let ((polygon (read-red-squares input-file)))
    (loop for i from 0 below (array-dimension polygon 0)
          maximize (loop for j from (1+ i) below (array-dimension polygon 0)
                         when (contains-rectangle? polygon (rectangle-edges (aref polygon i) (aref polygon j)))
                           maximize (rectangle-area (aref polygon i) (aref polygon j))))))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2025 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
