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
  (apply #'+ (extrapolate measurements :selector #'last-value)))

(defun sum-of-predicted-values (list-of-measurements &key predictor)
  (apply #'+ (mapcar predictor list-of-measurements)))

(defun day-09-part-1 (input-file)
  (-> input-file
    (read-oasis-report)
    (sum-of-predicted-values :predictor #'predict-next-value)))

(defun predict-previous-value (measurements)
  (let* ((extrapolations (extrapolate measurements :selector #'first))
         (val (first extrapolations)))
    (loop for x in (rest extrapolations)
          do (setf val (- x val)))
    val))

(defun day-09-part-2 (input-file)
  (-> input-file
    (read-oasis-report)
    (sum-of-predicted-values :predictor #'predict-previous-value)))

(defun day-09 ()
  (let ((f #p"9-input.txt"))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
