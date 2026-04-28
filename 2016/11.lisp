(in-package :aoc2016)

(defparameter test-input "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.")

(defun valid-state? (facility)
  (loop for (g . c) in (rest facility)
        never (and (/= g c)
                   (some (lambda (gc) (= (car gc) c)) (rest facility)))))

(defun new-positions (facility floor deltas)
  (loop for delta in deltas
        collect (loop for (g . c) in (rest facility) for i from 0
                      when (= g floor) collect (list i :car (+ delta floor))
                      when (= c floor) collect (list i :cdr (+ delta floor)))))


(defun next-states (facility)
  (let* ((elevator-floor (first facility))
         (deltas (remove-if-not (lambda (d) (<= 0 (+ elevator-floor d) 3)) '(1 -1))))
    (loop for dir in (new-positions facility elevator-floor deltas)
          append (loop for moves in (append (mapcar #'list dir) (combinations dir 2))
                       for arr = (make-array (1- (length facility)) :initial-contents (copy-tree (rest facility)))
                       for new-facility = (loop for (i pos new-floor) in moves
                                                do (if (eq pos :car)
                                                       (setf (car (aref arr i)) new-floor)
                                                       (setf (cdr (aref arr i)) new-floor))
                                                finally (return (cons new-floor (coerce arr 'list))))
                       when (valid-state? new-facility)
                         collect (cons (first new-facility)
                                       (sort (rest new-facility)
                                             (lambda (a b) (or (< (car a) (car b))
                                                          (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))))))

(defun read-facility (facility-string)
  (flet ((read-floor (line)
           (union (cl-ppcre:all-matches-as-strings "\\w*?-compatible" line)
                  (cl-ppcre:all-matches-as-strings "\\w*? generator" line))))
    (let* ((facility (mapcar #'read-floor (split-sequence:split-sequence #\Newline facility-string)))
           (floor-to-item (loop for i from 0 for floor in facility
                                append (loop for x in floor collect (cons i x)))))
      (cons 0 (loop for (generator chip) on (sort floor-to-item #'string< :key #'cdr) by #'cddr
                    collect (cons (car generator) (car chip)))))))

(defun make-goal-state (facility)
  (cons 3 (loop repeat (1- (length facility)) collect (cons 3 3))))

(defun count-elevator-steps (facility-string &optional extras)
  (let* ((facility (read-facility facility-string))
         (facility (if extras (append facility extras) facility))
         (goal-state (make-goal-state facility)))
    (flet ((goal? (state) (equal goal-state state)))
      (path-cost-so-far (a*-search (list (make-path :state facility)) #'goal? #'next-states
                                   (lambda (x y) (declare (ignore x y)) 1) (lambda (x) (declare (ignore x)) 1) #'equal)))))

(defun day-11-part-1 (input-file)
  (count-elevator-steps (uiop:read-file-string input-file)))

(defun day-11-part-2 (input-file)
  (count-elevator-steps (uiop:read-file-string input-file) (list '(0 . 0) '(0 . 0))))

(defun day-11 ()
  (let ((f (fetch-day-input-file 2016 11)))
    (values (day-11-part-1 f)
            (day-11-part-2 f))))
