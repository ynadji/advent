(in-package :aoc2024)

(defparameter test-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

;; 8400 = 94a + 22b
;; 5400 = 34a + 67b
;;
;;

(defun doubles (&rest ints)
  (mapcar (lambda (x) (coerce x 'double-float)) ints))

(defun wholeish? (x)
  (multiple-value-bind (n rem) (round x)
    (when (<= -0.001d0 rem 0.001d0) n)))

(defun solve-linear-system (x1 y1 x2 y2 X Y)
  (let* ((A (magicl:from-list (doubles x1 x2 y1 y2) '(2 2)))
         (b (magicl:from-list (doubles X Y) '(2 1)))
         (x (magicl:@ (magicl:inv A) b))
         (x-sol (wholeish? (magicl:tref x 0 0)))
         (y-sol (wholeish? (magicl:tref x 1 0))))
    ;(print x)
    (when (and x-sol y-sol)
      (cons x-sol y-sol))))

(defun read-claw-games (input-file &optional (part 1))
  (let ((lines (uiop:read-file-lines input-file))
        system
        systems)
    (loop for line in lines
          when (string= line "")
            do (push (reverse system) systems)
               (setf system nil)
          if (str:starts-with? "Button" line)
            do (cl-ppcre:register-groups-bind ((#'parse-integer a b))
                   ("X\\+(\\d+), Y\\+(\\d+)" line)
                 (push a system)
                 (push b system))
          else
            do (cl-ppcre:register-groups-bind ((#'parse-integer X Y))
                   ("X=(\\d+), Y=(\\d+)" line)
                 (if (= part 1)
                     (progn (push X system)
                            (push Y system))
                     (progn (push (+ 10000000000000 X) system)
                            (push (+ 10000000000000 Y) system))))
          finally
             (push (reverse system) systems))
    (reverse systems)))

(defun day-13-part-1 (input-file)
  (loop for system in (read-claw-games input-file)
        for (A . B) = (apply #'solve-linear-system system)
        when (and A B)
          sum (+ (* 3 A) B)))

(defun day-13-part-2 (input-file) (progn input-file -1)
  (loop for system in (read-claw-games input-file 2)
        for (A . B) = (apply #'solve-linear-system system)
        when (and A B)
          sum (+ (* 3 A) B)))

(defun day-13 ()
  (let ((f (fetch-day-input-file 2024 13)))
    (values (day-13-part-1 f)
            (day-13-part-2 f))))
