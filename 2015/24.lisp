(in-package :aoc2015)

(defparameter test-input "1
2
3
4
5
7
8
9
10
11")

(defun read-present-weights (input-file)
  (string-to-num-list (uiop:read-file-string input-file)))

;; each sum must equal (/ (reduce #'+ weights) 3)
;; of course it's NP-complete :)
;; https://aryan-wr.medium.com/3-way-partition-problem-40dbd846a37c
;;
;; we need to track both the sum and the product
(defun 3-partition% (weights bin-weight &optional
                                                (a1 0) (b1 0) (c1 0)
                                                (a2 1) (b2 1) (c2 1)
                                                (a3 0) (b3 0) (c3 0))
  (if (null weights)
      (when (and (= a1 b1 c1)
                 (and (< a3 b3)
                      (< a3 c3)))
        (list (list* a1 a2 a3) (list* b1 b2 b3) (list* c1 c2 c3)))
      (let ((x (first weights)))
        (remove nil
                (nconc (when (<= (+ x a1) bin-weight)
                         (3-partition% (rest weights) bin-weight (+ x a1) b1 c1 (* x a2) b2 c2 (1+ a3) b3 c3))
                       (when (<= (+ x b1) bin-weight)
                         (3-partition% (rest weights) bin-weight a1 (+ x b1) c1 a2 (* x b2) c2 a3 (1+ b3) c3))
                       (when (<= (+ x c1) bin-weight)
                         (3-partition% (rest weights) bin-weight a1 b1 (+ x c1) a2 b2 (* x c2) a3 b3 (1+ c3))))))))

(defun 3-partition (weights)
  (let ((bin-weight (/ (reduce #'+ weights) 3)))
    (3-partition% weights bin-weight)))

(defun day-24-part-1 (input-file)
  (let* ((weights (string-to-num-list (uiop:read-file-string input-file)))
         (sorted-groups (sort (remove-duplicates (partition (3-partition weights) 3) :test #'equal) #'< :key #'cddar)))
    (let ((min-legroom (cddaar sorted-groups)))
      (loop for (weight qe . num-packages) in (mapcar #'first sorted-groups)
            while (= num-packages min-legroom)
            minimize qe))))

(defun day-24-part-2 (input-file) (progn input-file -1))

(defun day-24 ()
  (let ((f (fetch-day-input-file 2015 24)))
    (values (day-24-part-1 f)
            (day-24-part-2 f))))
