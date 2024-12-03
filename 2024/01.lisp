(in-package :aoc2024)

;; trying out some dumb stuff to shave off a millisecond or two.

;; preloading saves ~1 ms.
(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *both-ids*
    (labels ((read-location-ids (lines)
               (unzip
                (mapcar (lambda (line) (mapcar #'parse-integer
                                               (cl-ppcre:split "\\s+" line)))
                        lines))))
      (let ((f (fetch-day-input-file 2024 1)))
        (read-location-ids (uiop:read-file-lines f))))))

(defun day-01-part-1 (both-ids)
  (declare (optimize (speed 3))) ; fractions of a millisecond.
  ;; possible measurement error, but LET can assign in parallel, while LOOP FORs
  ;; happen sequentially.
  (let ((xs (sort (first both-ids) #'<))
        ;; with optimize i see a complaint of:
        ;;
        ;; note: unable to
        ;;   optimize
        ;; due to type uncertainty:
        ;; The first argument is a SEQUENCE, not a LIST.
        ;;
        ;; if i change this to (the list (second both-ids)) it doesn't complain,
        ;; but it actually runs a bit slower than before.
        ;;
        ;; without THE LIST
        ;;
        ;; AOC2024> (time (loop repeat 10000 do (day-01-part-1 *both-ids*)))
        ;;Evaluation took:
        ;;  0.218 seconds of real time
        ;;  0.218297 seconds of total run time (0.211610 user, 0.006687 system)
        ;;  100.00% CPU
        ;;  1,310,704 bytes consed
        ;;
        ;;
        ;; with THE LIST
        ;;
        ;;AOC2024> (time (loop repeat 10000 do (day-01-part-1 *both-ids*)))
        ;;Evaluation took:
        ;;  0.307 seconds of real time
        ;;  0.307003 seconds of total run time (0.299618 user, 0.007385 system)
        ;;  100.00% CPU
        ;;  1,245,168 bytes consed
        (ys (sort (second both-ids) #'<)))
    (loop for x fixnum in xs for y fixnum in ys
          sum (abs (- x y)) fixnum)))

(defun day-01-part-2 (both-ids)
  (let ((freqs (serapeum:frequencies (second both-ids))))
    (loop for x in (first both-ids)
          sum (* x (gethash x freqs 0)))))

(defun day-01 ()
  (values (day-01-part-1 (copy-tree *both-ids*))
          (day-01-part-2 *both-ids*)))
