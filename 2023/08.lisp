(in-package :aoc2023)

(defun read-map (input-file)
  (let* ((lines (uiop:read-file-lines input-file))
         (route (car lines))
         (map-lines (cddr lines))
         (states (->> map-lines
                   (mapcar (lambda (line) (str:split " " line)))
                   (mapcar #'first)))
         (network (make-hash-table :test #'equal :size (length states))))
    (loop for map-line in map-lines for state in states do
      (let ((next-states (->> map-line
                           (str:split " = ")
                           (second)
                           (str:remove-punctuation)
                           (str:split " "))))
        (setf (gethash state network)
              next-states)))
    (values route
            network)))

(defun traverse-network (route start end network &key (part-2? nil))
  (let ((position start)
        (i -1)
        (route (coerce route 'list)))
    (circular! route)
    (loop for direction in route do
      (incf i)
      (if (or (equal position end)
              (and part-2? (str:ends-with? "Z" position)))
          (return i)
          (if (eq direction #\L)
              (setf position (first  (gethash position network)))
              (setf position (second (gethash position network))))))))

(defun day-08-part-1 (input-file)
  (multiple-value-bind (route network) (read-map input-file)
    (traverse-network route "AAA" "ZZZ" network)))

(defun day-08-part-2 (input-file)
  (multiple-value-bind (route network) (read-map input-file)
    (let ((starts (remove-if-not (lambda (state)
                                   (str:ends-with? "A" state))
                                 (alexandria:hash-table-keys network))))
      (apply #'lcm (mapcar (lambda (start)
                             (traverse-network route start "fake" network :part-2? t))
                           starts)))))

(defun day-08 ()
  (let ((f #p"08-input.txt"))
    (values (day-08-part-1 f)
            (day-08-part-2 f))))
