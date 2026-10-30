(in-package :aoc2017)

(defparameter test-input-1 "aa bb cc dd ee
aa bb cc dd aa
aa bb cc dd aaa")

(defparameter test-input-2 "abcde fghij
abcde xyz ecdab
a ab abc abd abf abj
iiii oiii ooii oooi oooo
oiii ioii iioi iiio")

(defun parse-passphrases (input-file)
  (mapcar (lambda (l) (uiop:split-string l)) (uiop:read-file-lines input-file)))

(defun sort-passphrase (passphrase)
  (mapcar (lambda (s) (sort s #'char<)) passphrase))

(defun day-04% (input-file)
  (flet ((all-distinct (passphrases)
           (ax:length= passphrases (remove-duplicates passphrases :test #'equal))))
    (let ((passphrases (parse-passphrases input-file)))
      (values (length (remove-if-not #'all-distinct passphrases))
              (length (remove-if-not #'all-distinct (mapcar #'sort-passphrase passphrases)))))))


(defun day-04 ()
  (let ((f (fetch-day-input-file 2017 4)))
    (day-04% f)))
