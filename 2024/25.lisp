(in-package :aoc2024)

(defparameter test-input "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")

(defun lock? (lines)
  (every (lambda (c) (char= c #\#)) (first lines)))

(defun read-key-lock (lines)
  (let ((rotated (rotate-90-clockwise lines)))
    (mapcar (lambda (line) (1- (count #\# line))) rotated)))

(defun fits? (key lock)
  (every (lambda (k l) (<= (+ k l) 5)) key lock))

(defun parse-keys-and-locks (keys-or-locks)
  (loop for kol in keys-or-locks
        for lines = (str:split #\Newline kol)
        if (lock? lines)
          collect (read-key-lock lines) into locks
        else
          collect (read-key-lock lines) into keys
        finally
           (return (values keys locks))))

(defun day-25-part-1 (input-file)
  (let ((keys-or-locks (str:split (format nil "~%~%") (str:trim (uiop:read-file-string input-file)))))
    (multiple-value-bind (keys locks) (parse-keys-and-locks keys-or-locks)
      (loop for key in keys sum
        (loop for lock in locks count (fits? key lock))))))

(defun day-25-part-2 (input-file) (progn input-file -1))

(defun day-25 ()
  (let ((f (fetch-day-input-file 2024 25)))
    (values (day-25-part-1 f)
            (day-25-part-2 f))))
