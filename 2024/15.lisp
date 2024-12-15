(in-package :aoc2024)

(defparameter test-input-me "########
#..O.O.#
#@O.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

>>>v")

(defparameter test-input-1 "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(defparameter test-input-2 "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(defparameter test-input-3 "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")

(defun char-to-direction (c)
  (ecase c (#\< :west) (#\v :south) (#\> :east) (#\^ :north)))

(defun widen-grid (s)
  (->> s
    (str:replace-all "#" "##")
    (str:replace-all "O" "[]")
    (str:replace-all "." "..")
    (str:replace-all "@" "@.")))

(defun read-robot-maze (input-file &optional (part 1))
  (destructuring-bind (maze moves) (str:split (coerce '(#\Newline #\Newline) 'string)
                                              (uiop:read-file-string input-file))
    (multiple-value-bind (grid starts)
        (if (= part 1)
            (parse-grid maze :starts? (lambda (c) (char= c #\@)))
            (parse-grid (widen-grid maze) :starts? (lambda (c) (char= c #\@))))
      (values grid starts (str:replace-all (format nil "~%") "" moves)))))

(defun dynamic-rotatef% (positions)
  (cons 'rotatef (mapcar (lambda (pos) `(paref grid (quote ,pos))) positions)))

(defmacro dynamic-rotatef (grid positions)
  `,(dynamic-rotatef% positions))

(defvar grid) ;; to ensure GRID is bound for the EVAL (lol. lmao.)
(defun robot-moves (grid moves robot-pos)
  ;; don't forget to move robot-pos!
  (loop for m across moves for direction = (char-to-direction m)
        for (next-chars next-positions) = (multiple-value-list (peek-to-boundary direction robot-pos grid))
        ;;do (print-grid grid)
        ;;do (format t "~a ~a ~a ~a~%" robot-pos direction next-chars next-positions)
        do (ecase (first next-chars)
             (#\. (rotatef (paref grid robot-pos) (paref grid (first next-positions)))
              (setf robot-pos (first next-positions)))
             (#\#)
             (#\O (let* ((idx (position-if (lambda (c) (member c '(#\. #\#))) next-chars)))
                    (ecase (nth idx next-chars)
                      (#\. (let ((posses (reverse (cons robot-pos (subseq next-positions 0 (1+ idx))))))
                             ;;(format t "~a~%" (dynamic-rotatef% posses))
                             (eval (dynamic-rotatef% posses))
                             (setf robot-pos (first next-positions))))
                      (#\#)))))))

(defun sum-box-gps (grid)
  (loop for i below (array-dimension grid 0) sum
        (loop for j below (array-dimension grid 1)
              when (char= #\O (aref grid i j))
                sum (+ (* 100 i) j))))

(defun day-15-part-1 (input-file)
  (multiple-value-bind (grid starts moves) (read-robot-maze input-file)
    (robot-moves grid moves (first starts))
    (sum-box-gps grid)))

(defun day-15-part-2 (input-file) (progn input-file -1))

(defun day-15 ()
  (let ((f (fetch-day-input-file 2024 15)))
    (values (day-15-part-1 f)
            (day-15-part-2 f))))
