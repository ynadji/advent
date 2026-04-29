(in-package :aoc2016)

(defun hash-to-nibble-list (s)
  (loop for x across (md5:md5sum-string s) collect (ldb (byte 4 4) x) collect (ldb (byte 4 0) x)))

(defun find-3-run (list)
  (let ((list (coerce list 'list)))
    (loop for (x y z) on list when (and (and x y z) (eql x y) (eql y z)) do (return x))))

(defun find-all-5-runs (list)
  (let ((list (coerce list 'list)))
    (loop for (a b c d e) on list when (and (and a b c d e)
                                            (eql a b)
                                            (eql b c)
                                            (eql c d)
                                            (eql d e)) collect a)))

(defun md5sum-string-to-hexstring (s)
  (string-downcase (str:join "" (map 'list (lambda (x) (format nil "~2,'0x" x)) (md5:md5sum-string s)))))

(defun hash (s &optional part2?)
  (if part2?
      (loop repeat 2017 with s = s do (setf s (md5sum-string-to-hexstring s)) finally (return s))
      (hash-to-nibble-list s)))

(defun find-64th-index (salt-prefix &optional part2?)
  (let ((triplets (make-hash-table))
        (keys '()))
    (loop for i from 0 for salt = (format nil "~a~a" salt-prefix i)
          for nibbles = (hash salt part2?)
          do 
             (ax:if-let (5-nibbles (remove-duplicates (find-all-5-runs nibbles)))
               (loop for nibble in 5-nibbles do
                 (ax:when-let ((index (gethash nibble triplets)))
                   (loop for idx in (reverse index) do
                     (when (<= (- i idx) 1000)
                       (pushnew idx keys)))
                   (remhash nibble triplets))))
             (ax:when-let (nibble (find-3-run nibbles))
               (push i (gethash nibble triplets)))
          when (>= (length keys) (if part2? 64 100)) ;; TODO: at this point do another 1K iterations _then_ return. and change back to
                                                     ;; 64.
            do (return))
    (nth 63 (sort keys #'<))))

(defun day-14% (input-file)
  (let ((prefix (str:trim (uiop:read-file-string input-file))))
    (values (find-64th-index prefix)
            (find-64th-index prefix t))))

(defun day-14 ()
  (let ((f (fetch-day-input-file 2016 14)))
    (day-14% f)))
