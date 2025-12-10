(in-package :aoc2025)

(defparameter test-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defclass machine ()
  ((goal :accessor goal :initarg :goal)
   (buttons :accessor buttons :initarg :buttons)
   (joltage :accessor joltage :initarg :joltage)
   (button-list :accessor button-list :initarg :button-list)))

(defmethod print-object ((m machine) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (goal buttons joltage button-list) m
      (format stream "~a ~a ~a ~a" goal buttons joltage button-list))))

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
           (button-list (mapcar #'string-to-num-list buttons))
           (buttons (mapcar (lambda (elts) (parse-button (first chunks) elts)) button-list)))
      (make-instance 'machine :goal goal :buttons buttons :joltage (string-to-num-list joltage)
                              :button-list button-list))))

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
            do (loop for next-button in (buttons machine)
                     unless (= button next-button)
                       do (queues:qpush q (list lights (1+ n) next-button))))))

(defun day-10-part-1 (input-file)
  (let ((machines (parse-machines input-file)))
    (loop for i from 0
          for machine in machines sum (minimum-presses machine)
          ;;do (format t "completed machine ~a~%" i)
          )))

(defun generate-linear-problem (joltage button-list)
  (values `(min (= n (+ ,@(loop for i from 1 for b in button-list collect (symb 'b i)))))
          (append (loop for jolt in joltage for i from 0
                        collect
                        `(= ,jolt
                            (+ ,@(loop for b from 1 for buttons in button-list
                                       when (member i buttons)
                                         collect (symb 'b b)))))
                  )))

(defun day-10-part-2 (input-file)
  (let ((machines (parse-machines input-file)))
    (loop for machine in machines
          for (objective constraints) = (multiple-value-list (generate-linear-problem (joltage machine) (button-list machine)))
          ;;do (format t "~a~%~a~%~a~%~%" machine objective constraints)
          ;;for solution = (lp:solve-problem (lp:parse-linear-problem objective constraints))
          sum (lp:solution-variable (lp:solve-problem (lp:parse-linear-problem objective (cons (cons 'integer (loop for b from 1 repeat (length (button-list machine)) collect (symb 'b b))) (filter-redundant-constraints constraints)))) 'N)
          ;;sum (lp:solution-variable solution 'N)
          )))

(defun day-10 ()
  (let ((f (fetch-day-input-file 2025 10)))
    (values (day-10-part-1 f)
            (day-10-part-2 f))))

;;; LLM stuff to get around redundancy limitation in LINEAR-PROGRAMMING.

(defun filter-redundant-constraints (constraints)
  "Returns a list of constraints with linearly dependent (redundant) rows removed."
  (let* ((parsed (mapcar #'parse-constraint constraints))
         ;; FIX: Access (second x) because we now use 'list' in parse-constraint
         (vars (remove-duplicates 
                (mapcan (lambda (x) (mapcar #'car (second x))) 
                        (copy-list parsed))
                :test #'equal))
         (matrix (build-matrix parsed vars))
         (independent-indices (find-independent-rows matrix)))
    ;; Return only the original constraints that were marked as independent
    (loop for i in independent-indices
          collect (nth i constraints))))

(defun parse-constraint (c)
  "Parses (= RHS (+ V1 V2 ...)) into (RHS ((V1 . 1) (V2 . 1)...))"
  ;; FIX: Use LIST instead of CONS to keep the structure (RHS TERMS)
  (let ((rhs (second c))
        (sum-expr (third c)))
    (list rhs 
          (loop for v in (cdr sum-expr)
                collect (cons v 1)))))

(defun build-matrix (parsed-constraints vars)
  "Converts parsed constraints into an M x (N+1) array."
  (let* ((m (length parsed-constraints))
         (n (length vars))
         (mat (make-array (list m (1+ n)) :initial-element 0.0 :element-type 'single-float)))
    (loop for i from 0
          for (rhs terms) in parsed-constraints  ;; FIX: Destructure (RHS TERMS)
          do (progn
               ;; Fill coefficients
               (loop for (v . coeff) in terms
                     for col-idx = (position v vars :test #'equal)
                     when col-idx
                     do (setf (aref mat i col-idx) (float coeff)))
               ;; Fill RHS (last column)
               (setf (aref mat i n) (float rhs))))
    mat))

(defun find-independent-rows (mat)
  "Performs Gaussian elimination and returns indices of non-redundant rows."
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims))
         (pivot-row 0)
         (row-indices (loop for i from 0 below rows collect i))
         (epsilon 1e-6)) ;; Use epsilon for float comparison
    
    (loop for col from 0 below (1- cols)
          while (< pivot-row rows) do
            ;; 1. Find a row with a non-zero value in this column
            (let ((pivot (loop for r from pivot-row below rows
                               if (> (abs (aref mat r col)) epsilon)
                               return r)))
              (when pivot
                ;; 2. Swap rows
                (dotimes (k cols)
                  (rotatef (aref mat pivot-row k) (aref mat pivot k)))
                (rotatef (nth pivot-row row-indices) (nth pivot row-indices))
                
                ;; 3. Normalize pivot row
                (let ((divisor (aref mat pivot-row col)))
                  (dotimes (k cols)
                    (setf (aref mat pivot-row k) (/ (aref mat pivot-row k) divisor))))
                
                ;; 4. Eliminate column in other rows
                (loop for r from 0 below rows
                      unless (= r pivot-row) do
                        (let ((factor (aref mat r col)))
                          (unless (< (abs factor) epsilon)
                            (dotimes (k cols)
                              (setf (aref mat r k) 
                                    (- (aref mat r k) (* factor (aref mat pivot-row k))))))))
                
                (incf pivot-row))))
    
    ;; 5. Return indices where the row is not empty (all zeros)
    ;; We only take the top 'pivot-row' number of rows because the rest are zeroed out.
    (subseq row-indices 0 pivot-row)))
