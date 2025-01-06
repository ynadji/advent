(in-package :aoc2015)

(defparameter test-input "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141")

(defun parse-distances (input-file)
  (labels ((parse-line (line)
             (destructuring-bind (c1 to c2 e dist) (str:split " " line)
               (declare (ignore to e))
               (list (sintern c1) (sintern c2) (parse-integer dist)))))
   (let (distances all-cities)
     (loop for line in (uiop:read-file-lines input-file)
           for (c1 c2 dist) = (parse-line line)
           do (push (cons (cons c1 c2) dist) distances)
              (push (cons (cons c2 c1) dist) distances)
              (push c1 all-cities)
              (push c2 all-cities)
           finally (return (values distances (remove-duplicates all-cities)))))))

(defun route-length (route distances)
  (loop for (c1 c2) on route
        while c2
        sum (ax:assoc-value distances (cons c1 c2) :test #'equal)))

(defun day-09-part-1 (input-file)
  (multiple-value-bind (distances all-cities) (parse-distances input-file)
    (let ((shortest-route-length most-positive-fixnum))
      (ax:map-permutations (lambda (route) (ax:minf shortest-route-length
                                               (route-length route distances)))
                           all-cities)
      shortest-route-length)))

(defun day-09-part-2 (input-file)
  (multiple-value-bind (distances all-cities) (parse-distances input-file)
    (let ((longest-route-length most-negative-fixnum))
      (ax:map-permutations (lambda (route) (ax:maxf longest-route-length
                                               (route-length route distances)))
                           all-cities)
      longest-route-length)))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2015 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
