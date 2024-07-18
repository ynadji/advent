(in-package :aoc2023)

(defun hash-score (s)
  (loop for c across s with value = 0
        do (setf value (mod (* 17 (+ (char-code c) value)) 256))
        finally (return value)))

(defun day-15-part-1 (input-file)
  (->> input-file uiop:read-file-string str:trim (str:split ",") (mapcar #'hash-score) (apply #'+)))

(defun parse-step (step)
  (let ((tmp (str:split "=|-" step :regex t :omit-nulls t)))
    (if (null (rest tmp))
        (values (first tmp) :minus 0)
        (values (first tmp) :equal (parse-integer (second tmp))))))

(defun focusing-power (box box-idx)
  (loop for lens in (reverse box) for slot# from 1 sum (* box-idx slot# (cdr lens))))

(defun day-15-part-2 (input-file)
  (let ((steps (->> input-file uiop:read-file-string str:trim (str:split ",")))
        (boxes (make-array 256 :initial-element nil)))
    (loop for step in steps
          do (multiple-value-bind (label op focal-length) (parse-step step)
               (let ((i (hash-score label)))
                 (if (eq op :minus)
                     (setf (aref boxes i) (remove label (aref boxes i) :test #'string= :key #'car))
                     (ax:if-let ((old-lens (assoc label (aref boxes i) :test #'string=)))
                       (setf (cdr old-lens) focal-length)
                       (push (cons label focal-length) (aref boxes i)))))))
    (loop for box across boxes for box-idx from 1 sum (focusing-power box box-idx))))

(defun day-15 ()
  (let ((f (fetch-day-input-file 2023 15)))
    (values (day-15-part-1 f)
            (day-15-part-2 f))))
