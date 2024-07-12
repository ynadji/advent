(in-package :aoc2023)

(defun ways-to-beat (time distance)
  (declare (fixnum time distance))
  (loop for my-time upto time
        count
        (let ((my-distance (* (- time my-time) my-time)))
          (> my-distance distance))))

(defun read-races (input-file)
  (let ((tmp (->> input-file
               (uiop:read-file-lines)
               (mapcar (lambda (s) (rest (str:split " " s :omit-nulls t)))))))
    (values (mapcar #'parse-integer (first tmp))
            (mapcar #'parse-integer (second tmp)))))

(defun read-as-one-race (input-file)
  (let ((tmp (->> input-file
               (uiop:read-file-lines)
               (mapcar (lambda (s) (rest (str:split " " s :omit-nulls t)))))))
    (values (parse-integer (apply #'str:concat (first tmp)))
            (parse-integer (apply #'str:concat (second tmp))))))

(defun day-06-part-1 (input-file)
  (multiple-value-bind (times distances) (read-races input-file)
    (apply #'*
           (loop for time in times for distance in distances
                 collecting (ways-to-beat time distance)))))

(defun day-06-part-2 (input-file)
  (multiple-value-bind (time distance) (read-as-one-race input-file)
    (ways-to-beat time distance)))

(defun day-06 ()
  (let ((f #p"6-input.txt"))
    (values (day-06-part-1 f)
            (day-06-part-2 f))))
