(in-package :aoc2025)

(defparameter test-input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(eval-when (:compile-toplevel :load-toplevel)
  (defun euclidean-distance (x1 y1 z1 x2 y2 z2)
    (sqrt (+ (expt (- x2 x1) 2)
             (expt (- y2 y1) 2)
             (expt (- z2 z1) 2))))

  (defun read-coordinates (input-file)
    (coerce (loop for line in (uiop:read-file-lines input-file)
                  collect (string-to-num-list line)) 'vector))

  (defmacro gethash-or-set-default (key ht default)
    (ax:with-gensyms (val)
      `(let ((,val (gethash ,key ,ht)))
         (if ,val ,val (setf (gethash ,key ,ht) ,default)))))

  (defstruct (coord (:print-function (lambda (c s d)
                                       (declare (ignore d))
                                       (format s "#< COORD ~a:~a,~a,~a >"
                                               (coord-jid c) (coord-x c) (coord-y c) (coord-z c)))))
    x y z jid)

  (defstruct edge c1 c2 distance)

  (defun pairwise-distances (coordinates)
    (let ((ht (make-hash-table))
          (seen (make-hash-table :test #'equal))
          edges)
      (flet ((seen-edge? (id1 id2)
               (gethash (cons (min id1 id2) (max id1 id2)) seen)))
        (loop for i below (length coordinates)
              for (x1 y1 z1) = (aref coordinates i)
              append (loop for j from (1+ i) below (length coordinates)
                           for (x2 y2 z2) = (aref coordinates j)
                           unless (or (= i j) (seen-edge? i j))
                             do (setf (gethash (cons (min i j) (max i j)) seen) t)
                                (push (make-edge :c1 (gethash-or-set-default i ht (make-coord :x x1 :y y1 :z z1 :jid i))
                                                 :c2 (gethash-or-set-default j ht (make-coord :x x2 :y y2 :z z2 :jid j))
                                                 :distance (euclidean-distance x1 y1 z1 x2 y2 z2))
                                      edges)))
        (values (sort edges #'< :key #'edge-distance) ht))))

  (let ((f (fetch-day-input-file 2025 8)))
    (multiple-value-bind (edges ht) (pairwise-distances (read-coordinates f))
      (defparameter *edges* edges)
      (defparameter *ht* ht))))

(defun num-circuits (circuits)
  (length (fast-remove-duplicates (loop for id from 0 below (length circuits)
                                        collect (disjoint-sets:disjoint-sets-find circuits id)))))

(defun day-08-part-1 (edges ht &optional (num-connections 10))
  (let ((circuits (disjoint-sets:make-disjoint-sets (hash-table-count ht))))
    (loop repeat num-connections for edge in edges
          for id1 = (coord-jid (edge-c1 edge))
          for id2 = (coord-jid (edge-c2 edge))
          unless (disjoint-sets:disjoint-sets-same-set-p circuits id1 id2)
            do (disjoint-sets:disjoint-sets-join circuits id1 id2))
    (let ((sorted-circuits (sort (frequencies (loop for id from 0 below (hash-table-count ht)
                                                    collect (disjoint-sets:disjoint-sets-find circuits id)))
                                 #'> :key #'cdr)))
      (* (cdr (first sorted-circuits))
         (cdr (second sorted-circuits))
         (cdr (third sorted-circuits))))))

(defun day-08-part-2 (edges ht)
  (let ((circuits (disjoint-sets:make-disjoint-sets (hash-table-count ht))))
    (loop for edge in edges for i from 0
          for id1 = (coord-jid (edge-c1 edge))
          for id2 = (coord-jid (edge-c2 edge))
          do (disjoint-sets:disjoint-sets-join circuits id1 id2)
          when (= 1 (num-circuits circuits))
            ;;when (= i 5243)
            return (* (coord-x (edge-c1 edge))
                      (coord-x (edge-c2 edge))))))

(defun day-08 ()
  (values (day-08-part-1 *edges* *ht* 1000)
          (day-08-part-2 *edges* *ht*)))
