(in-package :aoc2025)

(defparameter test-input "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(defparameter test-input-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(defvar *dp*)

(defun parse-device-paths (input-file)
  (let ((ht (make-hash-table)))
    (loop for line in (uiop:read-file-lines input-file)
          for parts = (uiop:split-string (remove #\: line))
          for label = (intern (string-upcase (first parts)) :keyword)
          do (loop for output in (rest parts)
                   do (push (intern (string-upcase output) :keyword) (gethash label ht))))
    ht))

(function-cache:defcached count-all-paths (dev &optional part2? dac? fft?)
  (cond ((eq dev :out) (if part2?
                           (if (and dac? fft?) 1 0)
                           1))
        (t (loop for next-dev in (gethash dev *dp*)
                 sum (count-all-paths next-dev part2? (or dac? (eq dev :dac)) (or fft? (eq dev :fft)))))))

(defun day-11 ()
  (let* ((f (fetch-day-input-file 2025 11))
         (*dp* (parse-device-paths f)))
    (values (count-all-paths :you)
            (count-all-paths :svr t))))
