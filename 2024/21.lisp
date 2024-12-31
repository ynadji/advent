(in-package :aoc2024)

(declaim (optimize (debug 3)))

(defparameter *keypad* '((:7 :8 :9) (:4 :5 :6) (:1 :2 :3) (:U :0 :A)))
(defparameter *key->pos* (loop for i from 0 for row in *keypad* append (loop for j from 0 for col in row collect (cons col (cons i j)))))

(defparameter *arrows* '((:U :^ :A) (:< :|v| :>)))
(defparameter *arrow->pos* (loop for i from 0 for row in *arrows* append (loop for j from 0 for col in row collect (cons col (cons i j)))))

(defparameter test-input "029A
980A
179A
456A
379A")

(defparameter test-output "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A")

(defun shortest-path (pos1 pos2 undef)
  (with-output-to-string (stream)
    (destructuring-bind (row-diff . col-diff)
        (pos- pos2 pos1)
      (declare (type fixnum row-diff col-diff))
      (when (and (not (equal undef (pos+ pos1 (cons 0 col-diff))))
                 (minusp col-diff))
        (dotimes (_ (abs col-diff))
          (write-char #\< stream)))
      (when (not (equal undef (pos+ pos1 (cons row-diff 0))))
        (dotimes (_ (abs row-diff))
          (if (minusp row-diff)
              (write-char #\^ stream)
              (write-char #\v stream))))
      (when (plusp col-diff)
        (dotimes (_ col-diff)
          (write-char #\> stream)))
      (when (equal undef (pos+ pos1 (cons row-diff 0)))
        (dotimes (_ (abs row-diff))
          (if (minusp row-diff)
              (write-char #\^ stream)
              (write-char #\v stream))))
      (when (and (equal undef (pos+ pos1 (cons 0 col-diff)))
                 (minusp col-diff))
        (dotimes (_ (abs col-diff))
          (write-char #\< stream))))
    (write-char #\A stream)))

(defun char->keyword (c)
  (intern (format nil "~a" c) :keyword))

(function-cache:defcached compute-path (path &optional (limit 2) (depth 0))
  (let* ((pad (if (zerop depth) *key->pos* *arrow->pos*))
         (start (ax:assoc-value pad :A))
         (undef (ax:assoc-value pad :U)))
    (loop for c across path for next = (ax:assoc-value pad (char->keyword c))
          for paths = (multiple-value-list (shortest-path start next undef))
          sum (loop for path in paths
                    if (= depth limit)
                    minimize (length path)
                    else
                    minimize (compute-path path limit (1+ depth)))
          do (setf start next))))

(defun day-21-part-1 (input-file)
  (let ((codes (uiop:read-file-lines input-file)))
    (loop for code in codes
          sum (* (compute-path code) (parse-integer code :junk-allowed t)))))

(defun day-21-part-2 (input-file)
  (let ((codes (uiop:read-file-lines input-file)))
    (loop for code in codes
          sum (* (compute-path code 25) (parse-integer code :junk-allowed t)))))

(defun day-21 ()
  (let ((f (fetch-day-input-file 2024 21)))
    (values (day-21-part-1 f)
            (day-21-part-2 f))))
