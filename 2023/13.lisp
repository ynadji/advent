(in-package :aoc2023)

;; refactor so it's pretty again :(
;; seemed fast enough with lists for my input size

(defun reflects? (mirror pivot)
  (let ((s1 (reverse (subseq mirror 0 pivot)))
        (s2 (subseq mirror pivot)))
    (when (every #'equal s1 s2)
      pivot)))

;; TODO: better LOOP?
(defun find-pivot-aux (mirror)
  (loop for pivot from 1 below (length mirror)
        do (when (reflects? mirror pivot)
             (return pivot))))

(defun find-pivot (mirror)
  (ax:if-let ((pivot (find-pivot-aux mirror)))
    (values (* pivot 100) :horizontal)
    (values (find-pivot-aux (transpose mirror)) :vertical)))

(defun find-all-pivots-aux (mirror)
  (loop for pivot from 1 below (length mirror)
        when (reflects? mirror pivot)
          collect pivot))

(defun find-all-pivots (mirror)
  (let ((all-pivots))
    (ax:when-let ((pivots (find-all-pivots-aux mirror)))
      (dolist (pivot pivots)
        (push (list (* pivot 100) :horizontal) all-pivots)))
    (ax:when-let ((pivots (find-all-pivots-aux (transpose mirror))))
      (dolist (pivot pivots)
        (push (list pivot :vertical) all-pivots)))
    all-pivots))

(defun sum-pivots (mirrors)
  (loop for mirror in mirrors sum (find-pivot mirror)))

(defun swap (c)
  (case c (#\# #\.) (#\. #\#)))

(defun smudge (mirror-row n new-c)
  (setf (char mirror-row n) new-c))

(defun sum-pivots-smudge (mirrors)
  (loop for mirror in mirrors for original-pivot-and-direction = (multiple-value-list (find-pivot mirror))
        for count = 0 sum
    (loop named mirror-loop for row in mirror for i below (length mirror) do
      (loop named by-col for j below (length row)
            for c across row do
              (progn
                (smudge row j (swap c))
                (let ((new-pivots (remove original-pivot-and-direction (find-all-pivots mirror) :test #'equal)))
                  (if (-> new-pivots null)
                      (smudge row j c)
                      (return-from mirror-loop (first (first new-pivots)))))))
          finally (return 0))))

(defun day-13-part-1 (input-file)
  (let ((mirrors (split-sequence:split-sequence "" (uiop:read-file-lines input-file) :test #'equal)))
    (sum-pivots mirrors)))

(defun print-mirror (mirror)
  (dolist (x mirror) (print x)))

(defun day-13-part-2 (input-file)
  (let ((mirrors (split-sequence:split-sequence "" (uiop:read-file-lines input-file) :test #'equal)))
    (sum-pivots-smudge mirrors)))

(defun day-13 ()
  (let ((f (fetch-day-input-file 2023 13)))
    (values (day-13-part-1 f)
            (day-13-part-2 f))))
