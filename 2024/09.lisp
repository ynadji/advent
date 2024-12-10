(in-package :aoc2024)

;; 00
(defparameter test-input "2333133121414131402")

(defun parse-disk-map (s)
  (let ((s (str:trim s)))
   (make-array (length s) :element-type 'fixnum
                          :initial-contents (loop for x across s collect (- (char-code x) 48)))))

(defun file? (x) (zerop (mod x 2)))
(defun free-space? (x) (not (file? x)))

;; ohh hmm. i think this assumes
(defun defrag (disk-map)
  ;; file id is just i or j / 2.
  (let ((i 0) (j (1- (array-dimension disk-map 0)))
        defragged)
    (loop
      while (<= i j)
      ;;do (format t "(~a, ~a)~%" i j)
      ;;do (print disk-map)
      when (file? i)
        do (loop repeat (aref disk-map i) do (push (/ i 2) defragged) (decf (aref disk-map i)))
           (incf i)                     ; points to 0s now
      when (and (free-space? i)
                (> (aref disk-map i) 0)
                (> (aref disk-map j) 0))
        ;; this will probably be faster if you only do the push in the loop and
        ;; do the math before the loop.
        do (loop repeat (min (aref disk-map j) (aref disk-map i))
                 do (decf (aref disk-map j))
                    (decf (aref disk-map i))
                    (push (/ j 2) defragged))
      when (zerop (aref disk-map j))
        do (decf j 2)
      when (zerop (aref disk-map i))
        do (incf i)
      finally (return (reverse defragged)))))

(defun day-09-part-1 (input-file)
  (loop for i from 0 for x in (defrag (parse-disk-map (uiop:read-file-string input-file))) sum (* i x)))

(defun make-empty-map (disk-map)
  (let ((empty-map (make-hash-table)))
    (loop for i from 1 below (array-dimension disk-map 0) by 2
          do (loop for size from 1 upto (aref disk-map i)
                   do (push i (gethash size empty-map)))
          finally
             (loop for k being the hash-key of empty-map
                   do (setf (gethash k empty-map) (reverse (gethash k empty-map))))
             (return empty-map))))

(defstruct file offset id size)

(defun file-defrag (disk-map)
  (let ((empty-map (make-empty-map disk-map))
        (i 0) (j (1- (array-dimension disk-map 0)))
        defragged)
    ;; maybe track the list by doing a list of (fileId, size)?
    (loop
      while (< i (array-dimension disk-map 0))
      when (file? i)
        do (loop repeat (aref disk-map i) do (push (cons (/ i 2)) defragged) (decf (aref disk-map i))))))

(defun day-09-part-2 (input-file) (progn input-file -1))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2024 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
