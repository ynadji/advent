(in-package :aoc2024)

;; 00
(defparameter test-input "2333133121414131402")

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

(defun make-empty-map (disk-map)
  (let ((empty-map (make-hash-table))
        (offset 0))
    (loop with i = 0
          while (< i (1- (array-dimension disk-map 0)))
          do (incf offset (aref disk-map i))
             (incf i)
             (loop for size from 1 upto (aref disk-map i)
                   do (push offset (gethash size empty-map)))
             (incf offset (aref disk-map i))
             (incf i)
          finally
             (loop for k being the hash-key of empty-map
                   do (setf (gethash k empty-map) (reverse (gethash k empty-map))))
             (return empty-map))))

(defstruct file offset id size)

(defun file-defrag (disk-map)
  (let ((empty-map (make-empty-map disk-map))
        (i 0) (j (1- (array-dimension disk-map 0)))
        (offset 0)
        used-free
        moved
        defragged)
    (loop
      do (format t "(~a, ~a) ~a~%" i j offset)
      do (format t "~a~%" disk-map)
      do (format t "~a~%~%" defragged)
      while (< i (array-dimension disk-map 0))
      when (free-space? i) ; can i fit the right-most file in the left-most free space?
        do (let ((free-spaces (gethash (aref disk-map j) empty-map)))
             (ax:if-let ((free-space (and (not (member offset used-free))
                                          (find offset free-spaces))))
               (progn (push (make-file :offset offset :id (/ j 2) :size (aref disk-map j)) defragged)
                      (incf offset (aref disk-map j))
                      (decf (aref disk-map i) (aref disk-map j)) ; decrement available free space
                      (setf (aref disk-map j) 0)
                      (decf j 2)
                      (when (> 0 (aref disk-map i))
                        (loop for size from 1 upto (aref disk-map i)
                              do (push offset (gethash size empty-map)))))
               (progn (decf j 2)
                      (incf offset (aref disk-map i))
                      (setf (aref disk-map i) 0)
                      (incf i))))
      when (file? i)
        do (push (make-file :offset offset :id (/ i 2) :size (aref disk-map i)) defragged)
           (incf offset (aref disk-map i))
           (decf (aref disk-map i) offset)
           (incf i)                     ; points to 0s now
      )))

(defun file-defrag2 (disk-map)
  (let ((empty-map (make-empty-map disk-map))
        (i 0) (j (1- (array-dimension disk-map 0)))
        (offset 0)
        used-free
        defragged)
    (loop
      do (format t "(~a, ~a) ~a~%" i j offset)
      do (format t "~a~%" disk-map)
      do (format t "~a~%~%" defragged)
      while (not (zerop j))
      do (let ((free-spaces (gethash (aref disk-map j) empty-map)))
           (ax:if-let ((free-space (loop for x in free-spaces when (not (member x used-free)) return x)))
             (progn (push (make-file :offset free-space :id (/ j 2) :size (aref disk-map j)) defragged)
                    (push free-space used-free)
                    
                    (incf offset (aref disk-map j))
                    (decf (aref disk-map i) (aref disk-map j)) ; decrement available free space
                    (setf (aref disk-map j) 0)
                    (decf j 2)
                    (when (> 0 (aref disk-map i))
                      (loop for size from 1 upto (aref disk-map i)
                            do (push offset (gethash size empty-map)))))
             (progn (decf j 2)
                    (incf offset (aref disk-map i))
                    (setf (aref disk-map i) 0)
                    (incf i)))))))

(defun day-09-part-2 (input-file) (progn input-file -1))

(defun day-09 ()
  (let ((f (fetch-day-input-file 2024 9)))
    (values (day-09-part-1 f)
            (day-09-part-2 f))))
