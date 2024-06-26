(ql:quickload :str)
(ql:quickload :uiop)
(ql:quickload :arrow-macros)

(defun read-schematic-from-file (f)
  (let ((lists (arrow-macros:->> f
                 (uiop:read-file-lines)
                 (mapcar #'(lambda (row) (append (coerce row 'list) '(#\.)))))))
    (make-array (list (length lists)
                      (length (first lists)))
                :initial-contents lists)))

(defun schematic-from-string (s)
  (let ((tmp (arrow-macros:->> s
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

(defun symbol-neighbors? (A i j)
  (arrow-macros:->>
      (neighbors A i j)
    (mapcar #'(lambda (ij) (aref A (car ij) (cdr ij))))
    (some #'symbol?)))

(defun rev-digits-to-num (curr-number-rev)
  (arrow-macros:-> curr-number-rev
    (reverse)
    (coerce 'string)
    (parse-integer)))

(defun solve-03-part-1 (input-file)
  (let* ((A (read-schematic-from-file input-file))
         (nrow (array-dimension A 0))
         (ncol (array-dimension A 1))
         (numbers nil))
    (loop for i below nrow do
      (let ((symbol-adjacent? nil)
            (curr-number-rev nil))
        (loop for j below ncol do
          (let ((x (aref A i j)))
            (cond
              ;; number
              ((number? x)
               (push x curr-number-rev)
               (when (symbol-neighbors? A i j)
                 (setq symbol-adjacent? T)))
              ;; end of number
              ((not-number? x)
               (when (and symbol-adjacent? curr-number-rev)
                 (push (rev-digits-to-num curr-number-rev) numbers))
               (setq curr-number-rev nil)
               (setq symbol-adjacent? nil)))))))
    (apply #'+ numbers)))