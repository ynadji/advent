(in-package :aoc2024)

;; 00
(defparameter test-input "2333133121414131402")

(defstruct file
  (id -1 :type fixnum)
  (offset 0 :type fixnum)
  (size 0 :type fixnum))

(defun parse-disk-map (s)
  (let ((s (str:trim s)))
    (make-array (length s) :element-type 'fixnum
                           :initial-contents (loop for x across s collect (- (char-code x) 48)))))

(defun show-disk-map (disk-map)
  (loop for i from 0 for x across disk-map
        do (if (zerop (mod i 2))
               (loop repeat x do (princ (/ i 2)))
               (loop repeat x do (princ #\.))))
  (terpri))

(defun file? (x) (zerop (mod x 2)))
(defun free-space? (x) (not (file? x)))

;; ohh hmm. i think this assumes
(defun defrag (disk-map)
  ;; file id is just i or j / 2.
  (let ((i 0) (j (1- (array-dimension disk-map 0)))
        (offset 0)
        defragged)
    (loop
      while (<= i j)
      ;;do (format t "(~a, ~a)~%" i j)
      ;;do (print disk-map)
      when (file? i)
        do (push (make-file :offset offset :id (/ i 2) :size (aref disk-map i)) defragged)
           (incf offset (aref disk-map i))
           (decf (aref disk-map i) offset)
           (incf i)                     ; points to 0s now
      when (and (free-space? i)
                (> (aref disk-map i) 0)
                (> (aref disk-map j) 0))
        ;; this will probably be faster if you only do the push in the loop and
        ;; do the math before the loop.
        do (let ((file (make-file :offset offset :id (/ j 2) :size 0)))
             (loop repeat (min (aref disk-map j) (aref disk-map i))
                   do (decf (aref disk-map j))
                      (decf (aref disk-map i))
                      (incf (file-size file)))
             (push file defragged)
             (incf offset (file-size file)))
      when (zerop (aref disk-map j))
        do (decf j 2)
      when (zerop (aref disk-map i))
        do (incf i)
      finally (return (reverse defragged)))))

(defun day-09-part-1 (input-file)
  (loop for file in (defrag (parse-disk-map (uiop:read-file-string input-file)))
               sum (loop repeat (file-size file) for i from (file-offset file) sum (* i (file-id file)))))

(defun parse-disk-map-2 (s)
  (let* ((s (str:trim s))
         (disk-map (make-array (length s)))
         (offset 0))
    (loop for i from 0 for c across s
          for x = (digit-char-p c)
          if (zerop (mod i 2))
            do (setf (aref disk-map i) (make-file :id (/ i 2) :offset offset :size x))
          else
            do (setf (aref disk-map i) (make-file :id -1 :offset offset :size x))
          do (incf offset x))
    disk-map))

(defun show-disk-map-2 (disk-map)
  (loop for file across disk-map
        do (if (minusp (file-id file))
               (loop repeat (file-size file) do (princ #\.))
               (loop repeat (file-size file) do (princ (file-id file)))))
  (terpri))

;; possible on one pass?
(defun file-defrag (disk-map)
  (declare (optimize (speed 3))
           (type (simple-array file (*)) disk-map))
  (loop for j from (1- (array-dimension disk-map 0)) downto 0 by 2
        for used = (aref disk-map j) do
          (loop for i from 1 below (array-dimension disk-map 0) by 2
                for free = (aref disk-map i)
                if (and (>= (file-size free) (file-size used))
                        (<= (file-offset free) (file-offset used)))
                  do (setf (file-offset used) (file-offset free))
                     (incf (file-offset free) (file-size used))
                     (decf (file-size free) (file-size used))))
  disk-map)

(defun day-09-part-2 (input-file)
  (loop for file across (file-defrag (parse-disk-map-2 (uiop:read-file-string input-file)))
        unless (minusp (file-id file))
          sum (loop repeat (file-size file) for i from (file-offset file) sum (* i (file-id file)))))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2024 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
