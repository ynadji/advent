(defun count-houses (path)
  (labels ((c->dir (c)
             (ecase c (#\^ :north) (#\> :east) (#\< :west) (#\v :south))))
    (let ((pos '(0 . 0)))
      (length (remove-duplicates
               (cons pos
                     (loop for c across path
                           collect (setf pos (advance (c->dir c) pos))))
               :test #'equal)))))

(defun count-houses2 (path)
  (labels ((c->dir (c)
             (ecase c (#\^ :north) (#\> :east) (#\< :west) (#\v :south))))
    (let ((pos1 (cons 0 0))
          (pos2 (cons 0 0)))
      (length (remove-duplicates
               (cons pos1
                     (loop for c across path for i from 0
                           if (zerop (mod i 2))
                           collect (setf pos1 (advance (c->dir c) pos1))
                           else
                           collect (setf pos2 (advance (c->dir c) pos2))))
               :test #'equal)))))

(defun day-3-part-1 (input-file)
  (loop for path in (uiop:read-file-lines input-file)
        sum (count-houses path)))

(defun day-3-part-2 (input-file)
  (loop for path in (uiop:read-file-lines input-file)
        sum (count-houses2 path)))
