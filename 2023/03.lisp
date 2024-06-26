(in-package :aoc2023)

(defstruct gear pos numbers)

(defun read-schematic-from-file (f)
  (let ((lists (->> f
                 (uiop:read-file-lines)
                 (mapcar #'(lambda (row) (append (coerce row 'list) '(#\.)))))))
    (make-array (list (length lists)
                      (length (first lists)))
                :initial-contents lists)))

(defun schematic-from-string (s)
  (let ((tmp (->> s
               (str:split #\Newline)
               (mapcar #'(lambda (row) (append (coerce row 'list) '(#\.)))))))
    (make-array (list (length tmp)
                    (length (first tmp)))
                :initial-contents tmp)))

(defun neighbors (A i j)
  (let ((maxrow (array-dimension A 0))
        (maxcol (array-dimension A 1))
        (all-indices (list (cons (1+ i) j)
                           (cons i (1+ j))
                           (cons (1+ i) (1+ j))

                           (cons (1- i) j)
                           (cons i (1- j))
                           (cons (1- i) (1- j))

                           (cons (1+ i) (1- j))
                           (cons (1- i) (1+ j)))))
    (let ((valid-indices
            (loop for (x . y) in all-indices
                  if (and (and (>= x 0) (< x maxrow))
                          (and (>= y 0) (< y maxcol)))
                    collect (cons x y))))
      valid-indices)))

(defun symbol? (c) (and (not (eq #\. c))
                        (not (digit-char-p c))))
(defun dot? (c) (eq #\. c))
(defun number? (c) (digit-char-p c))
(defun not-number? (c) (not (number? c)))

(defun symbol-neighbors (A i j)
  (->> (neighbors A i j)
    (mapcar #'(lambda (ij) (aref A (car ij) (cdr ij))))))

(defun symbol-neighbors? (A i j)
  (some #'symbol? (symbol-neighbors A i j)))

(defun gear-neighbors-pos (A i j)
  (->> (neighbors A i j)
    (mapcar #'(lambda (ij) (cons ij (aref A (car ij) (cdr ij)))))
    (remove-if-not (lambda (x) (eq #\* (cdr x))))
    (mapcar #'first)))

(defun rev-digits-to-num (curr-number-rev)
  (-> curr-number-rev
    (reverse)
    (coerce 'string)
    (parse-integer)))

(defun solve-day-03 (input-file)
  (let* ((A (read-schematic-from-file input-file))
         (nrow (array-dimension A 0))
         (ncol (array-dimension A 1))
         (numbers nil)
         (gears nil))
    (loop for i below nrow do
      (let ((symbol-adjacent? nil)
            (curr-number-rev nil)
            (adjacent-gears nil))
        (loop for j below ncol do
          (let ((x (aref A i j)))
            (cond
              ;; number
              ((number? x)
               (push x curr-number-rev)
               (when (symbol-neighbors? A i j)
                 (setq symbol-adjacent? T))
               (loop for ij in (gear-neighbors-pos A i j) do
                     (pushnew (make-gear :pos ij :numbers nil) adjacent-gears :test #'equal :key #'gear-pos)))
              ;; end of number
              ((not-number? x)
               (when (and symbol-adjacent? curr-number-rev)
                 (push (rev-digits-to-num curr-number-rev) numbers))
               (when (and adjacent-gears curr-number-rev)
                 (loop for gear in adjacent-gears do
                   (let ((g (car (member (gear-pos gear) gears :key #'gear-pos :test #'equal))))
                     (if g
                         (setf (gear-numbers g) (append (list (rev-digits-to-num curr-number-rev)) (gear-numbers g)))
                         (progn (setf (gear-numbers gear) (cons (rev-digits-to-num curr-number-rev) (gear-numbers gear)))
                                (push gear gears))))))
               (setq curr-number-rev nil)
               (setq symbol-adjacent? nil)
               (setq adjacent-gears nil)))))))
    (values (apply #'+ numbers)
            (->> gears
              (mapcar #'gear-numbers)
              (remove-if-not (lambda (x) (= 2 (length x))))
              (mapcar (lambda (xy) (apply #'* xy)))
              (apply #'+)))))

(defun day-03 ()
  (let ((f #p"03-input.txt"))
    (solve-day-03 f)))
