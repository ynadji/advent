(in-package :aoc2023)

(defun read-oasis-report (file-path)
  (->> (uiop:read-file-lines file-path)
    (mapcar (lambda (line)
              (->> line
                (str:split " ")
                (mapcar #'parse-integer))))))

(defun difference (measurements)
  (mapcar #'- (rest measurements) (butlast measurements)))

(defun last-value (xs) (first (last xs)))

(defun extrapolate (measurements &key (selector #'last-value))
  (labels ((aux (measurements acc)
             (if (every #'zerop measurements)
                 (cons 0 acc)
                 (aux (difference measurements)
                      (cons (funcall selector measurements) acc)))))
    (aux measurements nil)))

(defun predict-next-value (measurements)
  (apply #'+ (extrapolate measurements)))

(defun sum-of-predicated-values (list-of-measurements)
  (apply #'+ (mapcar #'predict-next-value list-of-measurements)))

(defun day-09-part-1 (input-file)
  (-> input-file
    (read-oasis-report)
    (sum-of-predicated-values)))

(defun predict-previous-value (measurements)
  (let ((val (first measurements)))
           (loop for x in (rest measurements)
                 do (setf val (- x val)))
           val))

(defun day-09-part-2 (input-file)
  (let* ((report (read-oasis-report input-file))
         (extrapolations (mapcar (lambda (measurements) (extrapolate measurements :selector #'first))
                                 report))
         (tmp (mapcar #'predict-previous-value extrapolations)))
    (apply #'+ tmp)))

(defun day-09 ()
  (let ((f #p"09-input.txt"))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
