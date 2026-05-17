(in-package :aoc2016)

(defparameter test-input "swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d")

(defun parse-operations% (string-op)
  (let ((op (subseq string-op 0 (position #\Space string-op))))
    (ax:eswitch (op :test #'string=)
      ("swap" (if (str:starts-with? "swap letter" string-op)
                  (let ((fields (str:split #\Space string-op)))
                    (list op (char (third fields) 0) (char (sixth fields) 0)))
                  (cons op (string-to-num-list string-op))))
      ("rotate" (if (str:starts-with? "rotate based" string-op)
                    (list op (char string-op (1+ (position #\Space string-op :from-end t))))
                    (let ((delta (first (string-to-num-list string-op))))
                      (list op (if (str:contains? "left" string-op) (* -1 delta) delta)))))
      ("move" (cons op (string-to-num-list string-op)))
      ("reverse" (cons op (string-to-num-list string-op))))))

(defun parse-operations (string-ops)
  (mapcar
   (lambda (string-op)
     (destructuring-bind (func arg1 &optional arg2) (parse-operations% string-op)
       (ax:eswitch (func :test #'string=)
         ("swap" (if (numberp arg1)
                     (lambda (s) (rotatef (char s arg1)
                                     (char s arg2)))
                     (lambda (s) (rotatef (char s (position arg1 s))
                                     (char s (position arg2 s))))))
         ("rotate" (if (numberp arg1)
                       (lambda (s) (nrotate s arg1))
                       (lambda (s) (nrotate s (1+ (let ((idx (position arg1 s)))
                                               (if (>= idx 4) (1+ idx) idx)))))))
         ("move" (lambda (s) (nmove s arg1 arg2)))
         ("reverse" (lambda (s) (nreverse-range s arg1 arg2))))))
   string-ops))

(defun nreverse-range (s start end)
  (loop with i = start and j = end
        while (< i j) do
          (rotatef (aref s i) (aref s j))
          (incf i) (decf j))
  s)

(defun nrotate (s n)
  (let* ((len (length s))
         (k (mod n len)))
    (when (and (plusp len) (plusp k))
      (nreverse-range s 0 (1- len))
      (nreverse-range s 0 (1- k))
      (nreverse-range s k (1- len)))
    s))

(defun nmove (s x y)
  (if (< x y)
      (loop for i from x below y do (rotatef (char s i) (char s (1+ i))))
      (loop for i from x above y do (rotatef (char s i) (char s (1- i)))))
  s)

(defun day-21-part-1 (input-file &optional (password (copy-seq "abcdefgh")))
  (loop for func in (parse-operations (uiop:read-file-lines input-file))
        do (funcall func password))
  password)

(defun day-21-part-2 (input-file &optional (scrambled-password (copy-seq "fbgdceah")))
  (let ((funcs (parse-operations (uiop:read-file-lines input-file))))
    (ax:map-permutations (lambda (password)
                           (let ((original-password (copy-seq password)))
                             (loop for func in funcs do (funcall func password))
                             (when (string= password scrambled-password)
                               (return-from day-21-part-2 original-password))))
                         scrambled-password)))

(defun day-21 ()
  (let ((f (fetch-day-input-file 2016 21)))
    (values (day-21-part-1 f)
            (day-21-part-2 f))))
