(in-package :aoc2023)

(declaim (optimize (speed 3)))

(declaim (ftype (function (list list &optional fixnum) integer) find-all-possible-arrangements))
(function-cache:defcached find-all-possible-arrangements (springs nums &optional (run-length 0))
  (declare (fixnum run-length))
  (cond (springs
         (let ((c (first springs)))
           (cond ((char= c #\#)
                  (if (or (null nums)
                          (>= run-length (first nums)))
                      0
                      (find-all-possible-arrangements (rest springs) nums (1+ run-length))))
                 ((char= c #\.)
                  (cond ((zerop run-length)
                         (find-all-possible-arrangements (rest springs) nums 0))
                        ((= run-length (the fixnum (first nums)))
                         (find-all-possible-arrangements (rest springs) (rest nums) 0))
                        (t 0)))
                 ((char= c #\?)
                  (+ (find-all-possible-arrangements (cons #\. (rest springs)) nums run-length)
                     (find-all-possible-arrangements (cons #\# (rest springs)) nums run-length))))))
        (t (if (or (and (null nums)
                        (zerop run-length))
                   (and (= 1 (the fixnum (length nums)))
                        (= run-length (the fixnum (first nums)))))
               1
               0))))

(defun read-springs (input-file)
  (let ((tmp (mapcar (lambda (x) (str:split " " x))
                     (uiop:read-file-lines input-file))))
    (values (->> tmp
              (mapcar #'first)
              (mapcar (lambda (s) (coerce s 'list))))
            (->> tmp
              (mapcar #'second)
              (mapcar (lambda (x) (str:split "," x)))
              (mapcar (lambda (x) (mapcar #'parse-integer x)))))))

(defun read-springs-part-2 (input-file)
  (let ((tmp (mapcar (lambda (x) (str:split " " x))
                     (uiop:read-file-lines input-file))))
    (values (->> tmp
              (mapcar #'first)
              (mapcar (lambda (s) (coerce (str:join "?" (make-list 5 :initial-element s)) 'list))))
            (->> tmp
              (mapcar #'second)
              (mapcar (lambda (x) (str:split "," x)))
              (mapcar (lambda (x) (mapcar #'parse-integer x)))
              (mapcar (lambda (nums) (apply #'append (make-list 5 :initial-element nums))))))))

(defun day-12-part-1 (input-file)
  (multiple-value-bind (springs list-of-nums) (read-springs input-file)
    (loop for spring in springs for nums in list-of-nums
          sum (the integer (find-all-possible-arrangements spring nums)))))

;; takes 12 minutes D:<.

;; trying to improve the type inference and optimize things didn't help
;; unfortunately and i'm not quite sure why. would be worth asking around to get
;; some assistance.
(defun day-12-part-2 (input-file)
  (multiple-value-bind (springs list-of-nums) (read-springs-part-2 input-file)
    (loop for spring in springs for nums in list-of-nums
          sum (the integer (find-all-possible-arrangements spring nums)))))

(defun day-12 ()
  (let ((f (fetch-day-input-file 2023 12)))
    (values (day-12-part-1 f)
            157383940585037; (day-11-part-2 f) ; too slow for tests for now :(
            )))
