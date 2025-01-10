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

(function-cache:defcached generate-possible-replacements (molecule old new &optional (start 0) acc)
  (declare (optimize (speed 3)))
  (multiple-value-bind (new-molecule changed?) (cl-ppcre:regex-replace old molecule new :start start)
    (if changed?
        (generate-possible-replacements molecule old new (+ start (length old))
                                        (cons (str:concat (subseq molecule 0 start) new-molecule) acc))
        acc)))

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
    (let ((mols (list "e")))
     (loop for n from 1
           do (setf mols (remove-duplicates (loop for mol in mols append (generate-all-possible-replacements mol rules)) :test #'equal))
              ;;(prin1 mols)
              ;;(terpri)
           when (member molecule mols :test #'equal)
             ;;return (values n mols molecule)
             return n))))

;; TODO: easy speedup is to only solve the constraints once and only do one pass
;; to compute the number of minimum containers. track the minimum and # of
;; solutions that fit the minimum in one-pass bing bong.
(defun day-19 ()
  (let ((f (fetch-day-input-file 2015 19)))
    (values (day-19-part-1 f)
            (day-19-part-2 f))))
