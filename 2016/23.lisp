(in-package :aoc2016)

(defparameter test-input "cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a")

(defun day-23 ()
  (let ((f (fetch-day-input-file 2016 23)))
    (values (day-12% f :a 7)
            ;; runs in ~39 seconds without multiply optimization so w/e d;D
            (day-12% f :a 12))))
