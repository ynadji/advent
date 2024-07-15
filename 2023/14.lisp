(in-package :aoc2023)

(defun roll-right (chars &optional (acc nil) (rocks nil) (dots nil))
  "Recurse over CHARS until we hit a #\#, then CONS the rolled order of that
group. Do one more CONS when we run out of CHARS and REVERSE FLATTEN."
  (if-let ((c (first chars)))
    (case c
      (#\. (roll-right (rest chars) acc rocks (cons c dots)))
      (#\O (roll-right (rest chars) acc (cons c rocks) dots))
      (#\# (roll-right (rest chars) (cons (append dots rocks (list #\#)) acc) nil nil)))
    (alexandria:flatten (reverse (cons (append dots rocks) acc)))))

(defun score-chars (chars)
  (loop for i from 1 for c in chars sum (if (char= c #\O) i 0)))

(defun score-row (row) (-> row (coerce 'list) roll-right score-chars))

(defun day-14-part-1 (input-file)
  (loop for row in (->> input-file uiop:read-file-lines (mapcar #'string-to-chars) rotate-90-clockwise)
        sum (score-row row)))

(defun cycle (list-of-chars)
  (->> list-of-chars
    rotate-90-clockwise ; north
    (mapcar #'roll-right)
    rotate-90-clockwise ; west
    (mapcar #'roll-right)
    rotate-90-clockwise ; south
    (mapcar #'roll-right)
    rotate-90-clockwise ; east
    (mapcar #'roll-right)))

(defun score-rows (rows)
  (loop for row in rows sum (score-chars row)))

(defun day-14-part-2 (input-file)
  (let ((northern-chars (->> input-file uiop:read-file-lines (mapcar #'string-to-chars)))
        (cache (make-hash-table :test #'equal :size 1024)))
    (loop for cycles from 0
          do (if-let ((prev-cycle (gethash northern-chars cache)))
               (progn
                 (loop repeat (mod (- 1000000000 cycles)
                                   (- cycles prev-cycle))
                       do (setf northern-chars (cycle northern-chars)))
                 (return (score-rows (rotate-90-clockwise northern-chars))))
               (progn (setf (gethash northern-chars cache) cycles)
                      (setf northern-chars (cycle northern-chars)))))))

(defun day-14 ()
  (let ((f (fetch-day-input-file 2023 14)))
    (values (day-14-part-1 f)
            (day-14-part-2 f))))
