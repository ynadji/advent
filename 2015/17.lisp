(in-package :aoc2015)

(defun solve-eggnog-csp (sizes &optional (n 150))
  (screamer:all-values
    (let* ((vars (mapcar (lambda (x) (screamer:an-integer-betweenv 0 1 (write-to-string x))) sizes))
           (products (mapcar (lambda (v size) (screamer:*v v size))
                             vars sizes)))
      (screamer:assert! (screamer:=v n (apply #'screamer:+v products)))
      (screamer:solution vars (screamer:static-ordering #'screamer:linear-force)))))

(defun day-17-part-1 (input-file)
  (length (solve-eggnog-csp (string-to-num-list (uiop:read-file-string input-file)))))

(defun day-17-part-2 (input-file)
  (let* ((sizes (string-to-num-list (uiop:read-file-string input-file)))
         (solutions (solve-eggnog-csp sizes))
         (minimum-containers (loop for solution in solutions minimize (apply #'+ solution))))
    (loop for solution in solutions
          count (= minimum-containers (apply #'+ solution)))))

;; TODO: easy speedup is to only solve the constraints once and only do one pass
;; to compute the number of minimum containers. track the minimum and # of
;; solutions that fit the minimum in one-pass bing bong.
(defun day-17 ()
  (let ((f (fetch-day-input-file 2015 17)))
    (values (day-17-part-1 f)
            (day-17-part-2 f))))
