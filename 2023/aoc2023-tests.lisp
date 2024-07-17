(in-package :cl-user)
(defpackage test-aoc2023
  (:use #:cl #:aoc2023)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is)
  (:export #:aoc2023))

(in-package :test-aoc2023)

(def-suite aoc2023)
(in-suite aoc2023)

;; NB: Captures TEST and IS from :FIVEAM. Evaluates DAY multiple times, but we
;; only use it below so it isn't super important. I would rather have this in
;; utils.lisp but it would search for IS/TEST in AOC2023 instead of
;; TEST-AOC2023. Wasn't sure how to fix that. Obviously only makes sense for my
;; inputs d;D
(defmacro make-aoc-tests (test-cases)
  `(progn
     ,@(loop for (day expect1 expect2) in test-cases
             collect `(test ,(aoc2023:symb 'test- day)
                        (time (multiple-value-bind (res1 res2) (,(aoc2023:symb 'day- (format nil "~2,'0d" day)))
                                (is (= res1 ,expect1))
                                (is (= res2 ,expect2))))))))

(make-aoc-tests ((1 55447 54706)
                 (2 2795 75561)
                 (3 551094 80179647)
                 (4 21213 8549735)
                 (5 650599855 1240035)
                 (6 1159152 41513103)
                 (7 249726565 251135960)
                 (8 16697 10668805667831)
                 (9 1637452029 908)
                 (10 6754 567)
                 (11 10885634 707505470642)
                 (12 7670 157383940585037)
                 (13 35210 31974)
                 (14 112048 105606)
                 (15 505459 228508)
                 ;(16 0 0)
                 ;(17 0 0)
                 ;(18 0 0)
                 ;(19 0 0)
                 ;(20 0 0)
                 ;(21 0 0)
                 ;(22 0 0)
                 ;(23 0 0)
                 ;(24 0 0)
                 (25 514786 0)))
