(in-package :aoc2015)

(defun nice? (s)
  (let ((vowels 0)
        double)
   (loop for c across s and prev-c = nil then c
         when (find c "aeiou")
         do (incf vowels)
         when (eq c prev-c)
         do (setf double t))
   (and (>= vowels 3)
        double
        (not (cl-ppcre:scan "ab|cd|pq|xy" s)))))

(defun day-05-part-1 (input-file)
  (loop for s in (uiop:read-file-lines input-file)
        count (nice? s)))

(defun new-nice? (s)
  (and (cl-ppcre:scan "(..).*?\\1" s)
       (cl-ppcre:scan "(.).\\1" s)))

(defun day-05-part-2 (input-file)
  (loop for s in (uiop:read-file-lines input-file)
        count (new-nice? s)))

(defun day-05 ()
  (let ((f (fetch-day-input-file 2015 5)))
    (values (day-05-part-1 f)
            (day-05-part-2 f))))
