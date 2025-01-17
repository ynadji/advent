(in-package :aoc2016)

(defun prefix-five-0s? (arr &optional part2)
  (when (and (zerop (aref arr 0))
             (zerop (aref arr 1))
             (< (aref arr 2) #x10))
    (if part2
        (values (aref arr 2)
                (string-downcase (format nil "~x" (ash (logand #xf0 (aref arr 3)) -4))))
        (string-downcase (format nil "~x" (aref arr 2))))))

(defun find-password (door-id)
  (str:join ""
            (loop with n = 0 for i from 0 for door-id-i = (format nil "~a~a" door-id i)
                  until (= n 8)
                  for next-char? = (prefix-five-0s? (md5:md5sum-string door-id-i))
                  when next-char?
                    collect next-char?
                    and do (incf n))))

(defun day-05-part-1 (input-file)
  (find-password (str:trim (uiop:read-file-string input-file))))

(defun find-password-2 (door-id)
  (let ((arr (make-array 8 :initial-element "x")))
   (loop with n = 0 for i from 0 for door-id-i = (format nil "~a~a" door-id i)
         until (= n 8)
         for (idx next-char?) = (multiple-value-list (prefix-five-0s? (md5:md5sum-string door-id-i) t))
         when (and idx (<= 0 idx 7) (string= "x" (aref arr idx)))
           do (setf (aref arr idx) next-char?)
              (incf n))
    (str:join "" (coerce arr 'list))))

(defun day-05-part-2 (input-file)
  (find-password-2 (str:trim (uiop:read-file-string input-file))))

;; TODO: add LPARALLEL i guess?
(defun day-05 ()
  (let ((f (fetch-day-input-file 2016 5)))
    (values (day-05-part-1 f)
            (day-05-part-2 f))))
