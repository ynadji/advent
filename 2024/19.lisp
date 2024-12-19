(in-package :aoc2024)

(declaim (optimize (speed 3)))

(defparameter test-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defun read-towel-designs (input-file)
  (let ((lines (uiop:read-file-lines input-file)))
    (values (btrie:make-trie (str:split ", " (first lines)))
            (rest (rest lines)))))

(defun possible-design? (trie design)
  (declare (type simple-string design))
  (if (str:empty? design)
      t
      (loop for end from (length design) downto 1
            for seq = (btrie:obtain-seq trie (subseq design 0 end))
              thereis (and seq
                           (btrie:wordp seq)
                           (possible-design? trie (subseq design end))))))

(defun day-19-part-1 (input-file)
  (multiple-value-bind (trie designs) (read-towel-designs input-file)
    (loop for design in designs count (possible-design? trie design))))

;; TODO: Figure out how to pass this into LPARALLEL
(defparameter *trie* (read-towel-designs #P "19-input.txt"))

(function-cache:defcached possible-design-3? (design)
  (declare (type simple-string design))
  (if (str:empty? design)
      1
      (loop for end from 1 upto (length design)
            for seq = (btrie:obtain-seq *trie* (subseq design 0 end))
            
            for res = (and seq
                           (btrie:wordp seq)
                           (possible-design-3? (subseq design end)))
            when res sum res)))

(defun day-19-part-2 (input-file)
  (multiple-value-bind (trie designs) (read-towel-designs input-file)
    (let ((lparallel:*kernel* (lparallel:make-kernel 8)))
      (reduce #'+ (remove nil (lparallel:pmapcar #'possible-design-3? designs))))))

(defun day-19 ()
  (let ((f (fetch-day-input-file 2024 19)))
    (values (day-19-part-1 f)
            (day-19-part-2 f))))
