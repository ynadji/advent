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
          for label = (symb (string-upcase (first parts)))
          do (loop for output in (rest parts)
                   do (push (symb (string-upcase output)) (gethash label ht))))
    ht))

(function-cache:defcached find-all-paths% (dev &optional part2? dac? fft?)
  (cond ((eq dev 'out) (if part2?
                           (when (and dac? fft?) 'out)
                           'out))
        (t (loop for next-dev in (gethash dev *dp*)
                 collect (find-all-paths% next-dev part2? (or dac? (eq dev 'dac)) (or fft? (eq dev 'fft)))))))

(defun count-all-paths (start &optional part2?)
  (count 'out (ax:flatten (find-all-paths% start part2?))))

(defun day-11-part-1 (input-file)
  (let ((*dp* (parse-device-paths input-file)))
    (count-all-paths 'you)))

(defun day-11-part-2 (input-file)
  (let ((*dp* (parse-device-paths input-file)))
    (count-all-paths 'svr t)))

(defun day-11 ()
  (let ((f (fetch-day-input-file 2025 11)))
    (values (day-11-part-1 f)
            (day-11-part-2 f))))
