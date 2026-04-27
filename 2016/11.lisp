(in-package :aoc2016)

;; TODO: switch to using aoc-utils. copy new cartesian-product func to aoc-utils.
;; add paip-search to aoc-utils.

(defparameter test-input "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.")

;; elevator contains at most two items. must have at least one.
;; movement is: elevator, x1[, x2] up or down one floor. costs 1.
;; valid state: no floor has unpaired chip with other generator
;; TODO: would this be cleaner if FACILITY was an FSET2:SEQ?
(defstruct (facility (:type list)) floor-1 floor-2 floor-3 floor-4)
(defun facility-equal (f1 f2)
  (and (fset2:equal? (facility-floor-1 f1) (facility-floor-1 f2))
       (fset2:equal? (facility-floor-2 f1) (facility-floor-2 f2))
       (fset2:equal? (facility-floor-3 f1) (facility-floor-3 f2))
       (fset2:equal? (facility-floor-4 f1) (facility-floor-4 f2))))

(defun print-facility (facility &key (stream t))
  (labels ((relabel (x)
             (let ((c (char-upcase (char x 0))))
               (if (str:contains? "generator" x)
                   (format nil "~cG" c)
                   (format nil "~cM" c))))
           (make-ht (items)
             (ax:alist-hash-table (loop for i from 0 for x in items collect (cons x i)) :test #'equal))
           (print-floor (f ht)
             (let* ((empty-floor (make-array (hash-table-count ht) :initial-element ". ")))
               (fset2:do-set (x (fset2:less f "elevator"))
                 (let ((label (relabel x)))
                   (setf (aref empty-floor (gethash label ht)) label)))
               (format stream (if (fset2:contains? f "elevator") "E  " ".  "))
               (format stream "~{~a~^ ~}~%" (coerce empty-floor 'list)))))
    (let* ((items (fset2:convert 'list (fset2:less (reduce #'fset2:union facility) "elevator")))
           (items (sort items #'string<))
           (ht (make-ht (mapcar #'relabel items))))
      (format stream "F4 ")
      (print-floor (facility-floor-4 facility) ht)
      (format stream "F3 ")
      (print-floor (facility-floor-3 facility) ht)
      (format stream "F2 ")
      (print-floor (facility-floor-2 facility) ht)
      (format stream "F1 ")
      (print-floor (facility-floor-1 facility) ht))))

(defun chip? (generator-or-chip)
  (str:ends-with? "-compatible" generator-or-chip))

(defun generator? (generator-or-chip)
  (not (or (chip? generator-or-chip)
           (string= generator-or-chip "elevator"))))

(defun find-pair (generator-or-chip)
  (destructuring-bind (element flag) (uiop:split-string generator-or-chip :separator '(#\- #\Space))
    (if (string= "generator" flag)
        (format nil "~a-compatible" element)
        (format nil "~a generator" element))))

(defun valid-floor? (floor)
  (flet ((chip-ok? (chip)
           (let ((generator (find-pair chip)))
             (or (fset:contains? floor generator)
                 (fset2:empty? (fset2:less (fset2:filter #'generator? floor) generator))))))
    (let ((chips (fset2:filter #'chip? floor)))
      (fset2:every #'chip-ok? chips))))

(defun valid-state? (facility)
  (every #'valid-floor? facility))

(defun next-states (facility)
  ;;(declare (optimize debug))
  (flet ((to-set (l) (fset2:convert 'fset2:set l))
         (add-elevator (s) (fset2:with s "elevator")))
    (let* ((elevator-floor (position-if (lambda (f) (fset2:contains? f "elevator")) facility))
           (possible-floor-indices (remove-if-not #'(lambda (x) (<= 0 x 3)) (list (1- elevator-floor) (1+ elevator-floor))))
           (items (fset2:convert 'list (fset2:less (nth elevator-floor facility) "elevator")))
           ;; hopefully not too bad because the sizes are very small
           (items-to-move (remove-duplicates (mapcar #'add-elevator (mapcar #'to-set (cartesian-product items items)))
                                             :test #'fset2:equal?)))
      ;;(print items-to-move)
      (loop for n in possible-floor-indices
            append
            (loop for movers in items-to-move for new-facility = (copy-facility facility)
                  do (fset2:set-differencef (nth elevator-floor new-facility) movers)
                     (fset2:unionf (nth n new-facility) movers)
                  when (valid-state? new-facility)
                    collect new-facility)))))

(defun read-facility (facility-string)
  (flet ((read-floor (line)
           (fset2:convert 'fset2:set (union (cl-ppcre:all-matches-as-strings "\\w*?-compatible" line)
                                           (cl-ppcre:all-matches-as-strings "\\w*? generator" line)))))
    (let ((facility (mapcar #'read-floor (split-sequence:split-sequence #\Newline facility-string))))
      (fset:includef (facility-floor-1 facility) "elevator")
      facility)))

(defun make-goal-state (facility)
  (list (fset2:empty-set) (fset2:empty-set) (fset2:empty-set) (reduce #'fset2:union facility)))

(defun count-elevator-steps (facility-string)
  (let* ((facility (read-facility facility-string))
         (goal-state (make-goal-state facility)))
    (flet ((goal? (state) (facility-equal goal-state state)))
      ;;(graph-search (list facility) #'goal? #'next-states #'prepend #'facility-equal)
      (path-cost-so-far (a*-search (list (make-path :state facility)) #'goal? #'next-states (lambda (x y) 1) (lambda (x) 1) #'facility-equal)))))

(defun day-11-part-1 (input-file)
  (count-elevator-steps (uiop:read-file-string input-file)))

(defun day-11-part-2 (input-file) (progn input-file -1))

(defun day-11 ()
  (let ((f (fetch-day-input-file 2016 11)))
    (values (day-11-part-1 f)
            (day-11-part-2 f))))
