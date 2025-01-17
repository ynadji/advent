(in-package :cl-user)
(defpackage test-aoc2016
  (:use #:cl #:aoc2016)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is)
  (:export #:aoc2016))

(in-package :test-aoc2016)

(def-suite aoc2016)
(in-suite aoc2016)

;; NB: Captures TEST and IS from :FIVEAM. Evaluates DAY multiple times, but we
;; only use it below so it isn't super important. I would rather have this in
;; utils.lisp but it would search for IS/TEST in AOC2016 instead of
;; TEST-AOC2016. Wasn't sure how to fix that. Obviously only makes sense for my
;; inputs d;D
(defmacro make-aoc-tests (test-cases)
  `(progn
     ,@(loop for (day expect1 expect2) in test-cases
             collect `(test ,(aoc2016:symb 'test- day)
                        (time (multiple-value-bind (res1 res2) (,(aoc2016:symb 'day- (format nil "~2,'0d" day)))
                                (is (equal res1 ,expect1))
                                (is (equal res2 ,expect2))))))))

(make-aoc-tests ((1 242 150)
                 (2 "36629" "99C3D")
                 (3 1032 1838)
                 (4 409147 991)
                 (5 "f97c354d" "863dde27")
                 (6 "wkbvmikb" "evakwaga")
                 ;;(7 956 40149)
                 ;;(8 1333 2046)
                 ;;(9 141 736)
                 ;;(10 329356 4666278)
                 ;;(11 "cqjxxyzz" "cqkaabcc")
                 ;;(12 111754 65402)
                 ;;(13 733 725)
                 ;;(14 2655 1059)
                 ;;(15 222870 117936)
                 ;;(16 103 405)
                 ;;(17 654 57)
                 ;;(18 768 781)
                 ;;(19 535 212)
                 ;;(20 776160 786240)
                 ;;(21 111 188)
                 ;;(22 1824 1937)
                 ;;(23 255 334)
                 ;;(24 11266889531 77387711)
                 ;;(25 8997277 0)
                 ))
