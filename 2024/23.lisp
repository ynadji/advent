(in-package :aoc2024)

(defparameter test-input "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(defun read-lan (input-file)
  (let ((lan (make-hash-table :test #'equal)))
    (loop for line in (uiop:read-file-lines input-file)
          for (x y) = (str:split "-" line)
          do (push y (gethash x lan))
          (push x (gethash y lan)))
    lan))

(defun adjacent? (lan v1 v2)
  (and (member v2 (gethash v1 lan) :test #'equal)
       (member v1 (gethash v2 lan) :test #'equal)))

(defun find-triangle (lan &optional (c #\t))
  (length
   (remove-duplicates
    (loop for name in (ax:hash-table-keys lan)
          for neighbors = (gethash name lan)
          ;;do (format t "~a: ~a~%" name neighbors)
          when (char= c (char name 0))
          append (loop for v1 in neighbors
                       append
                       (loop for v2 in neighbors
                             when (and (string/= v1 v2)
                                       (adjacent? lan v1 v2))
                             collect (sort (list name v1 v2) #'string<))))
    :test #'equal)))

(defun day-23-part-1 (input-file)
  (find-triangle (read-lan input-file)))

(defparameter *longest-clique* nil)
(defun bron-kerbosch (lan r p x)
  (if (and (null p) (null x) (> (length r) (length *longest-clique*)))
      (setf *longest-clique* r) ;; can i do this without SETF?
      (loop for v in (copy-list p)
            for neighbors = (gethash v lan)
            collect (bron-kerbosch lan
                                   (cons v r)
                                   (intersection p neighbors :test #'equal)
                                   (intersection x neighbors :test #'equal))
            do (ax:deletef p v :test #'equal)
            (push v x))))

(defun day-23-part-2 (input-file)
  (let ((lan (read-lan input-file)))
    (bron-kerbosch lan nil (ax:hash-table-keys lan) nil)
    (str:join "," (sort *longest-clique* #'string<))))

(defun day-23 ()
  (let ((f (fetch-day-input-file 2024 23)))
    (values (day-23-part-1 f)
            (day-23-part-2 f))))
