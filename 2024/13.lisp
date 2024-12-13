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
    (when (and x-sol y-sol)
      (cons x-sol y-sol))))

(defun read-claw-games (input-file &optional (part 1))
  (loop for system in (mapcar #'string-to-num-list (str:split (coerce '(#\Newline #\Newline) 'string)
                                                              (uiop:read-file-string input-file)))
        when (= part 2)
          do (incf (nth 4 system) 10000000000000)
             (incf (nth 5 system) 10000000000000)
        collect system))

(defun day-13% (input-file part)
  (loop for system in (read-claw-games input-file part)
        for (A . B) = (apply #'solve-linear-system system)
        when (and A B)
          sum (+ (* 3 A) B)))

(defun day-13 ()
  (let ((f (fetch-day-input-file 2024 13)))
    (values (day-13% f 1)
            (day-13% f 2))))
