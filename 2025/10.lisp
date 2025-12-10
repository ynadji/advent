(in-package :aoc2025)

(defparameter test-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defclass machine ()
  ((goal :accessor goal :initarg :goal)
   (buttons :accessor buttons :initarg :buttons)
   (joltage :accessor joltage :initarg :joltage)))

(defmethod print-object ((m machine) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (goal buttons joltage) m
      (format stream "~a ~a ~a" goal buttons joltage))))

#+or(defun prune-buttons (goal buttons)
  (let ((light-indices (loop for i from 0 for c across goal when (char= c #\#) collect i)))
    (loop for pset in (powerset buttons)
          for flat = (ax:flatten pset)
          collect flat)))

(defun parse-machine (line)
  (flet ((parse-lights (s)
           (parse-integer (substitute #\0 #\. (substitute #\1 #\# (str:trim s :char-bag '(#\[ #\]))))
                          :radix 2))
         (parse-button (lights elements)
           (let ((s (make-string (- (length lights) 2) :initial-element #\0)))
             (loop for i in elements do (setf (char s i) #\1))
             (parse-integer s :radix 2))))
    (let* ((chunks (str:split #\Space line))
           (goal (parse-lights (first chunks)))
           (joltage (ax:lastcar chunks))
           (buttons (subseq chunks 1 (1- (length chunks))))
           (buttons (mapcar (lambda (elts) (parse-button (first chunks) elts)) (mapcar #'string-to-num-list buttons))))
      (make-instance 'machine :goal goal :buttons buttons :joltage joltage))))

(defun parse-machines (input-file)
  (mapcar #'parse-machine (uiop:read-file-lines input-file)))

(defun minimum-presses (machine)
  (let ((q (queues:make-queue :simple-queue))
        (seen (make-hash-table)))
    (loop for button in (buttons machine)
          ;; assumes at least one button press must be done, which holds for my input.
          do (queues:qpush q (list 0 1 button)))
    (setf (gethash 0 seen) t)
    (loop while (plusp (queues:qsize q))
          for (prev-lights n button) = (queues:qpop q)
          for lights = (logxor prev-lights button)
          when (= lights (goal machine))
            ;;do #+or(room) #+or(format t "~a~%" (queues:qsize q)) and
            return n
          unless (gethash lights seen)
            do (loop for button in (buttons machine)
                     do (queues:qpush q (list lights (1+ n) button))))))

(defun day-10-part-1 (input-file)
  (let ((machines (parse-machines input-file)))
    (loop for i from 0
          for machine in machines sum (minimum-presses machine)
          ;;do (format t "completed machine ~a~%" i)
          )))

(defun day-10-part-2 (input-file) (progn input-file -1))

(defun day-10 ()
  (let ((f (fetch-day-input-file 2025 10)))
    (values (day-10-part-1 f)
            (day-10-part-2 f))))
