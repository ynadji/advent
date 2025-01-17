(in-package :aoc2016)

(defvar *keypad* '(((0 . 0) . #\1) ((0 . 1) . #\2) ((0 . 2) . #\3)
                   ((1 . 0) . #\4) ((1 . 1) . #\5) ((1 . 2) . #\6)
                   ((2 . 0) . #\7) ((2 . 1) . #\8) ((2 . 2) . #\9)))
(defvar *keypad-part-2* '(((0 . 2) . #\1)
                          ((1 . 1) . #\2) ((1 . 2) . #\3) ((1 . 3) . #\4)
                          ((2 . 0) . #\5) ((2 . 1) . #\6) ((2 . 2) . #\7) ((2 . 3) . #\8) ((2 . 4) . #\9)
                          ((3 . 1) . #\A) ((3 . 2) . #\B) ((3 . 3) . #\C)
                          ((4 . 2) . #\D)))

(defvar test-input "ULL
RRDDD
LURDL
UUUUD")

(defun day-02% (input-file keypad)
  (let ((instructions (uiop:read-file-lines input-file))
        digit)
    (coerce
     (loop with pos = (if (eq keypad *keypad*) (cons 1 1) (cons 2 0))
           for inst in instructions
           do (loop for c across inst for dir = (ecase c (#\U :north) (#\D :south) (#\L :west) (#\R :east))
                    do (ax:when-let ((new-pos (advance dir pos)))
                         (when (ax:assoc-value keypad new-pos :test #'equal)
                           (setf pos new-pos))))
              (setf digit (ax:assoc-value keypad pos :test #'equal))
           when digit
             collect digit)
     'string)))

(defun day-02 ()
  (let ((f (fetch-day-input-file 2016 2)))
    (values (day-02% f *keypad*)
            (day-02% f *keypad-part-2*))))
