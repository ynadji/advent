(in-package :aoc2023)

(defun scratchcard-num-wins (line)
  (let* ((winners-and-haves (-<>> line
                              (str:replace-first "Card \\d+: " "" <> :regex T)
                              (str:split " | ")))
         (winners (str:split " " (first winners-and-haves) :omit-nulls T))
         (haves (str:split " " (second winners-and-haves) :omit-nulls T)))
    (length (intersection winners haves :test #'string=))))

(defun scratchcard-points (line)
  (let ((num-wins (scratchcard-num-wins line)))
      (if (zerop num-wins)
          0
          (expt 2 (1- num-wins)))))

(defun day-04-part-1 (input-file)
  (apply #'+ (mapcar #'scratchcard-points (uiop:read-file-lines input-file))))

(defstruct card (points 0) (count 1))

(defun scratchcard-total (lines)
  (let* ((cards (make-array (length lines) :element-type 'card
                                           :initial-element (make-card))))
    (loop for line in lines
          for idx from 0 do
            (setf (aref cards idx) (make-card :points (scratchcard-num-wins line)
                                              :count 1)))
    (loop for i from 0 below (length cards) do
      (loop for j from 1 to (card-points (aref cards i)) do
        (let ((curr-card (aref cards i))
              (next-card (aref cards (+ i j))))
          (setf (card-count next-card)
                (+ (card-count curr-card)
                   (card-count next-card))))))
    cards))

(defun day-04-part-2 (input-file)
  (->> input-file
    (uiop:read-file-lines)
    (scratchcard-total)
    (map 'list #'card-count)
    (apply #'+)))

(defun day-04 ()
  (let ((f #p"4-input.txt"))
    (values (day-04-part-1 f)
            (day-04-part-2 f))))
