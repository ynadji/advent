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

(defun parse-machine (line)
  (let* ((chunks (str:split #\Space line))
         (goal (str:trim (first chunks) :char-bag '(#\[ #\])))
         (joltage (ax:lastcar chunks))
         (buttons (subseq chunks 1 (1- (length chunks))))
         (buttons (mapcar #'string-to-num-list buttons)))
    (make-instance 'machine :goal goal :buttons buttons :joltage joltage)))

(defun parse-machines (input-file)
  (mapcar #'parse-machine (uiop:read-file-lines input-file)))

(defun toggle-lights (lights button)
  (flet ((toggle (c)
           (if (char= c #\#) #\. #\#)))
   (let ((lights (copy-seq lights)))
     (loop for b in button do (setf (char lights b) (toggle (char lights b))))
     lights)))

(defun minimum-presses (machine)
  (let ((q (queues:make-queue :simple-queue)))
    (loop for button in (buttons machine)
          ;; assumes at least one button press must be done, which holds for my input.
          do (queues:qpush q (list* (make-string (length (goal machine)) :initial-element #\.) 1 button)))
    (loop while (plusp (queues:qsize q))
          for state = (queues:qpop q)
          for lights = (toggle-lights (first state) (cddr state))
          when (string= lights (goal machine))
            do (room) (format t "~a~%" (queues:qsize q)) and return (second state)
          do (loop for button in (buttons machine)
                   do (queues:qpush q (list* lights (1+ (second state)) button))))))

(defun day-10-part-1 (input-file)
  (let ((machines (parse-machines input-file)))
    (loop for i from 0
          for machine in machines sum (minimum-presses machine)
          do (format t "completed machine ~a~%" i))))

(defun day-10-part-2 (input-file) (progn input-file -1))

(defun day-10 ()
  (let ((f (fetch-day-input-file 2025 10)))
    (values (day-10-part-1 f)
            (day-10-part-2 f))))
