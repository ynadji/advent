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

(defun n-partition% (weights bin-weight &optional (a 0) (b 1) (c 0))
  (if (null weights)
      (when (and (= a bin-weight))
        (list a b c))
      (let ((x (first weights)))
        (remove nil
         (nconc (when (<= (+ x a) bin-weight)
                 (n-partition% (rest weights) bin-weight (+ x a) (* x b) (1+ c)))
               (n-partition% (rest weights) bin-weight a b c))))))

(defun n-partition (weights n)
  (let ((bin-weight (/ (reduce #'+ weights) n)))
    (n-partition% weights bin-weight)))

(defun day-24% (input-file n)
  (let* ((weights (string-to-num-list (uiop:read-file-string input-file)))
         (sorted-groups (sort (remove-duplicates (partition (n-partition weights n) 3) :test #'equal) #'< :key #'third)))
    (let ((min-legroom (caddar sorted-groups)))
      (loop for (weight qe num-packages) in sorted-groups
            while (= num-packages min-legroom)
            minimize qe))))

;; very slow, but clean and it works. according to profiling, i spend 87%! of
;; the time just doing the final PARTITION call to group things into threes wtf?
(defun day-24 ()
  (let ((f (fetch-day-input-file 2015 24)))
    (values (day-24% f 3)
            (day-24% f 4))))
