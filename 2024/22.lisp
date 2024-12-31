(in-package :aoc2024)

(defparameter test-input "1
2
3
2024")

(declaim (optimize (speed 3)))

(defun mix (secret n)
  (declare (optimize (speed 3))
           (type fixnum secret n))
  (logxor secret n))

(defun prune (secret)
  (declare (optimize (speed 3))
           (type fixnum secret))
  (mod secret 16777216))

(defun evolve% (secret)
  (declare (optimize (speed 3))
           (type fixnum secret))
  (setf secret (prune (mix secret (the fixnum (* 64 secret)))))
  (setf secret (prune (mix secret (the fixnum (floor (/ secret 32))))))
  (setf secret (prune (mix secret (* 2048 secret)))))

(defun evolve (secret n)
  (declare (type fixnum secret n))
  (loop repeat n do (setf secret (evolve% secret)) finally (return secret)))

(defun generate (secret n)
  (declare (type fixnum secret n))
  (loop repeat n do (setf secret (evolve% secret)) collect (mod secret 10)))

(defun deltas (secret n)
  (declare (type fixnum secret n))
  (let ((prices (generate secret n)))
    (values (mapcar #'- prices (cons (mod secret 10) prices))
            prices)))

(defun day-22-part-1 (input-file)
  (loop for secret in (string-to-num-list (uiop:read-file-string input-file))
        sum (evolve secret 2000)))

(defun day-22-part-2 (input-file)
  (let ((bananas (make-hash-table :test #'equal :size (* 2000 4))))
    (loop for secret in (string-to-num-list (uiop:read-file-string input-file))
          for (deltas prices) = (multiple-value-list (deltas secret 2000))
          do
          (loop for (p1 p2 p3 p4) on prices
                for (d1 d2 d3 d4) on deltas
                for four-deltas = (list d1 d2 d3 d4)
                with seen = (make-hash-table :test #'equal)
                while p4
                unless (gethash four-deltas seen)
                do (setf (gethash four-deltas seen) t)
                (incf (the fixnum (gethash four-deltas bananas 0)) (the fixnum p4))
                ;(format t "adding ~a at ~a~%" four-deltas p4)
                ))
    (cdr (first (sort (ax:hash-table-alist bananas) #'> :key #'cdr)))))

(defun day-22 ()
  (let ((f (fetch-day-input-file 2024 22)))
    (values (day-22-part-1 f)
            (day-22-part-2 f))))
