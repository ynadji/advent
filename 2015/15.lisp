(in-package :aoc2015)

(defparameter test-input "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")

(defun parse-ingredients (input-file)
  (mapcar #'string-to-num-list (uiop:read-file-lines input-file)))

(defun test-id ()
  (loop for x upto 100
        append (loop for y upto 100 when (= 100 (+ x y)) collect (list x y))))

(defun ingredient-distribution ()
  (loop for w upto 100
        append (loop for x upto 100
                     append (loop for y upto 100
                                  append (loop for z upto 100
                                               when (= 100 (+ w x y z)) collect (list w x y z))))))

(defun calc-properties (prop n)
  (mapcar (lambda (x) (* x n)) (subseq prop 0 4)))

(defun calc-properties-2 (prop n)
  (mapcar (lambda (x) (* x n)) prop))

;; TODO: cleanup differences between part1/2. can probably get some easy speed
;; optimizations with types here.
(defun highest-scoring-cookie (properties distribution)
  (loop for dist in distribution
        maximize
        (apply #'*
               (mapcar (lambda (x) (if (minusp x) 0 x))
                       (apply #'mapcar #'+
                              (loop for prop in properties for i from 0
                                    collect (calc-properties prop (nth i dist))))))))

(defun highest-scoring-cookie-restrict-calories (properties distribution)
  (loop for dist in distribution
        maximize
        (let ((vals (mapcar (lambda (x) (if (minusp x) 0 x))
                            (apply #'mapcar #'+
                                   (loop for prop in properties for i from 0
                                         collect (calc-properties-2 prop (nth i dist)))))))
          (if (= 500 (ax:lastcar vals))
              (apply #'* (subseq vals 0 4))
              0))))

(defun day-15-part-1 (input-file)
  (highest-scoring-cookie (parse-ingredients input-file) (ingredient-distribution)))

(defun day-15-part-2 (input-file)
  (highest-scoring-cookie-restrict-calories (parse-ingredients input-file) (ingredient-distribution)))

(defun day-15 ()
  (let ((f (fetch-day-input-file 2015 15)))
    (values (day-15-part-1 f)
            (day-15-part-2 f))))
