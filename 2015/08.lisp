(in-package :aoc2015)

(defun num-chars-code-and-memory (s)
  (let ((s-unhex (remove #\\ (str:replace-all "\\\\" "!"
                                              (str:replace-all "\\\\\x[0-9a-f]{2}" "!" s :regex t)))))
    (list (length s)
          ;; subtract leading/trailing \"
          (- (length s-unhex) 2))))

(defun encoded-length (s)
  (length (with-output-to-string (stream) (prin1 s stream))))

(defun day-08-part-1 (input-file)
  (loop for s in (uiop:read-file-lines input-file)
        sum (apply #'- (num-chars-code-and-memory s))))

(defun day-08-part-2 (input-file)
  (loop for s in (uiop:read-file-lines input-file)
        sum (- (encoded-length s) (first (num-chars-code-and-memory s)))))

(defun day-08 ()
  (let ((f (fetch-day-input-file 2015 8)))
    (values (day-08-part-1 f)
            (day-08-part-2 f))))
