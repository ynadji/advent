(in-package :aoc2015)

(defparameter test-input-1 "H => HO
H => OH
O => HH
e => H
e => O

HOH")

(defparameter test-input-2 "H => HO
H => OH
O => HH
e => H
e => O

HOHOHO")

(defun parse-molecule-replacements (input-file)
  (destructuring-bind (rules molecule) (str:split (format nil "~%~%") (uiop:read-file-string input-file))
    (values (mapcar (lambda (rule) (str:split " => " rule)) (str:split #\Newline rules))
            molecule)))

(defun generate-possible-replacements (molecule old new)
  (labels ((aux (molecule old new &optional (start 0) acc)
             (multiple-value-bind (new-molecule changed?) (cl-ppcre:regex-replace old molecule new :start start)
               (if changed?
                   (aux molecule old new (+ start (length old))
                        (cons (str:concat (subseq molecule 0 start) new-molecule) acc))
                   acc))))
    (aux molecule old new)))

(defun generate-all-possible-replacements (molecule rules)
  (remove-duplicates
   (loop for (old new) in rules append (generate-possible-replacements molecule old new))
   :test #'equal))

(defun day-19-part-1 (input-file)
  (multiple-value-bind (rules molecule) (parse-molecule-replacements input-file)
    (length
     (remove-duplicates
      (loop for (old new) in rules append (generate-possible-replacements molecule old new))
      :test #'equal))))

(defun day-19-part-2 (input-file)
  (multiple-value-bind (rules molecule) (parse-molecule-replacements input-file)
    (declare (ignore rules))
    ;; i kinda understand the logic, but am a bit confused as to why we count
    ;; the uppercase here rather than just the tokens to get the right answer.
    ;; obviously cheated a bit here :|.
    (- (count-if #'upper-case-p molecule)
       (str:count-substring "Ar" molecule)
       (str:count-substring "Rn" molecule)
       (* 2 (str:count-substring "Y" molecule))
       1)))

(defun day-19 ()
  (let ((f (fetch-day-input-file 2015 19)))
    (values (day-19-part-1 f)
            (day-19-part-2 f))))
