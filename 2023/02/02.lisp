(ql:quickload "str")

(defvar *loaded-1* '(("red" . 12) ("green" . 13) ("blue" . 14)))

(defun game-possible? (pulls)
  (loop for (color . count) in pulls
        always (<= count (cdr (assoc color *loaded-1* :test #'string=)))))

(defun split-subset (s)
  (let ((fields (str:split " " s)))
    (cons (cadr fields)
          (parse-integer (car fields)))))

;; TODO: how would i improve this so i don't need to repeat
;; this basic parsing here and in part-2-aux?
(defun parse-game (line)
  (let* ((stripped (str:replace-first "Game " "" line))
         (split (str:split "[:,;] " stripped :regex T))
         (game-id (parse-integer (car split)))
         (pulls (mapcar #'split-subset (cdr split))))
    (if (game-possible? pulls)
        game-id
        0)))

(defun part-2-aux (line)
  (let* ((stripped (str:replace-first "Game " "" line))
         (split (str:split "[:,;] " stripped :regex T))
         (pulls (mapcar #'split-subset (cdr split)))
         (colors (remove-duplicates (mapcar #'car pulls) :test #'string=)))
    (apply #'* (loop for color in colors
                     collect (loop for (c . n) in (remove-if-not #'(lambda (x) (string= x color)) pulls :key #'car)
                                   maximizing n)))))

(defun part-1 (input-file)
  (with-open-file (stream input-file)
    (loop for line = (read-line stream nil)
          while line
          sum (parse-game line))))

(defun part-2 (input-file)
  (with-open-file (stream input-file)
    (loop for line = (read-line stream nil)
          while line
          sum (part-2-aux line))))
