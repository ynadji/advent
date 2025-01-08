(in-package :aoc2015)

;;(declaim (optimize (debug 3)))

(defparameter test-input "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

(defun fly (kps fly-time rest-time total-time)
  (if (zerop total-time)
      0
      (multiple-value-bind (divisor remainder) (floor total-time (+ fly-time rest-time))
        (if (plusp divisor)
            (let* ((distance (* divisor kps fly-time))
                   (time-spent (* divisor (+ rest-time fly-time))))
              (+ distance
                 (fly kps fly-time rest-time (- total-time time-spent))))
            (* (min fly-time remainder)
               kps)))))

(defun day-14-part-1 (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        for (kps fly-time rest-time) = (string-to-num-list line)
        maximize (fly kps fly-time rest-time 2503)))

(defun all-argmax (arr)
  (let ((max (nth-value 1 (aops:argmax arr))))
    (loop for i from 0 for x across arr when (= max x) collect i)))

(defun day-14-part-2 (input-file &optional (n 2503))
  (let* ((reindeers (loop for line in (uiop:read-file-lines input-file)
                          collect (string-to-num-list line)))
         (current-reindeers (make-array (length reindeers) :initial-contents (mapcar #'copy-list reindeers)))
         (original-reindeers (make-array (length reindeers) :initial-contents reindeers))
         (scores (make-array (length reindeers) :initial-element 0))
         (distances (make-array (length reindeers) :initial-element 0)))
    (loop for second from 1 upto n
          do (loop for i from 0 below (array-dimension current-reindeers 0)
                   when (and (zerop (second (aref current-reindeers i)))
                             (zerop (third (aref current-reindeers i))))
                     do (setf (aref current-reindeers i)
                              (copy-list (aref original-reindeers i)))
                   if (plusp (second (aref current-reindeers i)))
                     do (incf (aref distances i) (first (aref current-reindeers i)))
                        (decf (second (aref current-reindeers i)))
                   else
                     do (decf (third (aref current-reindeers i))))
             (loop for j in (all-argmax distances)
                   do (incf (aref scores j))))
    (values (nth-value 1 (aops:argmax scores))
            scores
            distances)))

(defun day-14 ()
  (let ((f (fetch-day-input-file 2015 14)))
    (values (day-14-part-1 f)
            (day-14-part-2 f))))
