(in-package :aoc2015)

(defun char-inc (c)
  (code-char (+ 97 (mod (- (1+ (char-code c)) 97) 26))))

(defun chars-inc (chars)
  (declare (optimize (speed 3)))
  (labels ((aux (chars acc carry? first?)
             (if (null chars)
                 acc
                 (let ((c (first chars)))
                   (if (or carry? first?)
                       (let ((c+ (char-inc c)))
                         (aux (rest chars) (cons c+ acc) (char= c+ #\a) nil))
                       (aux (rest chars) (cons c acc) nil nil))))))
    (aux (reverse chars) nil nil t)))

(defun valid-password? (password)
  (labels ((bad-char? (c)
             (when c
               (member c '(#\i #\o #\l)))))
    (let (has-increasing? has-bad-char? pairs)
      (loop for (c1 c2 c3) on password
            for i from 0
            when (and (and c1 c2 c3)
                      (char= (char-inc (char-inc c1))
                             (char-inc c2)
                             c3)
                      (char< c1 c2 c3))
              do (setf has-increasing? t)
            when (or (bad-char? c1) (bad-char? c2) (bad-char? c3))
              do (setf has-bad-char? t)
            when (eq c1 c2)
              do (push (cons c1 c2) pairs))
      (and has-increasing?
           (not has-bad-char?)
           (>= (length (remove-duplicates pairs :test #'equal)) 2)))))

(defun next-password (password)
  (let ((password (coerce password 'list)))
    (loop do (setf password (chars-inc password))
          until (valid-password? password)
          finally (return (coerce password 'string)))))

(defun day-11 ()
  (let* ((password (str:trim (uiop:read-file-string (fetch-day-input-file 2015 11))))
         (password% (next-password password)))
    (values password%
            (next-password password%))))
