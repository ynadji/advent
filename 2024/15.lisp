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

(defparameter test-input-4 "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^")

(defparameter test-input-5 "##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.....@....##
##############

^^^^")

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

(defun robot-push (grid positions)
  (let ((chars (mapcar (lambda (pos) (paref grid pos)) positions)))
    (loop for pos in positions for c in (cons (ax:lastcar chars) (butlast chars))
          do (setf (paref grid pos) c))))

(defun robot-moves (grid moves robot-pos &optional write-file?)
  (labels ((pushable? (positions)
             (loop for pos in positions for idx from 0
                   when (char= (paref grid pos) #\.)
                     return idx)))
    (loop for m across moves for direction = (char-to-direction m)
          for (next-chars next-positions) = (multiple-value-list (peek-to-boundary direction robot-pos grid '(#\# #\.)))
          for num-instruction from 0
          when write-file?
            do (with-open-file (stream (format nil "15/~5,'0d.txt" num-instruction) :direction :output :if-exists :supersede)
                 (print-grid grid :stream stream :char-color-alist '((#\@ . :green) (#\[ . :blue) (#\] . :blue) (#\# . :black))))
          do (ecase (first next-chars)
               (#\.
                (rotatef (paref grid robot-pos) (paref grid (first next-positions)))
                (setf robot-pos (first next-positions)))
               (#\#)
               (#\O (ax:when-let ((idx (pushable? next-positions)))
                      (robot-push grid (first (get-all-box-columns grid direction robot-pos)))
                      (setf robot-pos (first next-positions))))
               ((#\[ #\])
                (if (member direction '(:north :south))
                    (let* ((all-next-positions (get-all-box-columns grid direction robot-pos))
                           (all-idxes (mapcar #'pushable? all-next-positions)))
                      (when (every #'identity all-idxes)
                        (loop for all-posses in all-next-positions for idx in all-idxes
                              do (robot-push grid (subseq all-posses 0 (1+ idx))))
                        (setf robot-pos (first next-positions))))
                    (ax:when-let ((idx (pushable? next-positions)))
                      (robot-push grid (cons robot-pos next-positions))
                      (setf robot-pos (first next-positions)))))))))

(defun get-all-box-columns% (grid direction positions &optional acc-positions)
  (if (null positions)
      acc-positions
      (let ((c (paref grid (first positions))))
        (if (member c '(#\[ #\]))
            (let* ((next-pos (advance (if (char= c #\[) :east :west) (first positions)))
                   (new-positions (nth-value 1 (peek-to-boundary direction next-pos grid '(#\. #\#)))))
              (append (get-all-box-columns% grid direction new-positions
                                            (cons (cons next-pos new-positions) acc-positions))
                      (get-all-box-columns% grid direction (rest positions) acc-positions)))
            (get-all-box-columns% grid direction (rest positions) acc-positions)))))

(defun get-all-box-columns (grid direction pos)
  (let ((positions (nth-value 1 (peek-to-boundary direction pos grid '(#\. #\#)))))
    (remove-duplicates
     (sort (remove-duplicates (get-all-box-columns% grid direction positions (list (cons pos positions)))
                              :test #'equal)
           #'< :key #'length)
     :test (lambda (x y) (subsetp x y :test #'equal)))))

(defun sum-box-gps (grid)
  (loop for i below (array-dimension grid 0)
        sum
        (loop for j below (array-dimension grid 1)
              when (member (aref grid i j) '(#\O #\[))
                sum (+ (* 100 i) j))))

(defun day-15% (input-file part)
  (multiple-value-bind (grid starts moves) (read-robot-maze input-file part)
    (robot-moves grid moves (first starts))
    (sum-box-gps grid)))

(defun day-15 ()
  (let ((f (fetch-day-input-file 2024 15)))
    (values (day-15% f 1)
            (day-15% f 2))))
