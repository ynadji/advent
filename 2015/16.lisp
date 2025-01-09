(in-package :aoc2015)

(defparameter mfcsam
  (serapeum:dict "children" 3
                 "cats" 7
                 "samoyeds" 2
                 "pomeranians" 3
                 "akitas" 0
                 "vizslas" 0
                 "goldfish" 5
                 "trees" 3
                 "cars" 2
                 "perfumes" 1))

(defun parse-sue (sue-string)
  (let* ((fields (str:split " " (str:replace-all ":" "" sue-string))))
    (loop for (key val) on fields by #'cddr collect
          (cons key (parse-integer val :junk-allowed t)))))

(defun sue? (fields &optional part2)
  (labels ((aux? (key val part2)
             (if part2
                 (let ((ht-val (gethash key mfcsam)))
                  (ax:switch (key :test #'equal)
                    ("cats" (when ht-val (< ht-val val)))
                    ("trees" (when ht-val (< ht-val val)))
                    ("pomeranians" (when ht-val (> ht-val val)))
                    ("goldfish" (when ht-val (> ht-val val)))
                    (otherwise (eq val ht-val))))
                 (eq val (gethash key mfcsam)))))
    (loop for (key . val) in fields
          always (aux? key val part2))))

(defun day-16% (input-file &optional part2)
  (loop for sue-string in (uiop:read-file-lines input-file)
        for sue = (parse-sue sue-string)
        when (sue? (subseq sue 1) part2)
          return (cdar sue)))

(defun day-16 ()
  (let ((f (fetch-day-input-file 2015 16)))
    (values (day-16% f)
            (day-16% f t))))
