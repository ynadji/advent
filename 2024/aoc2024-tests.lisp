(in-package :cl-user)
(defpackage test-aoc2024
  (:use #:cl #:aoc2024)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is)
  (:export #:aoc2024))

(in-package :test-aoc2024)

(def-suite aoc2024)
(in-suite aoc2024)

;; NB: Captures TEST and IS from :FIVEAM. Evaluates DAY multiple times, but we
;; only use it below so it isn't super important. I would rather have this in
;; utils.lisp but it would search for IS/TEST in AOC2024 instead of
;; TEST-AOC2024. Wasn't sure how to fix that. Obviously only makes sense for my
;; inputs d;D
(defmacro make-aoc-tests (test-cases)
  `(progn
     ,@(loop for (day expect1 expect2) in test-cases
             collect `(test ,(aoc2024:symb 'test- day)
                            (time (multiple-value-bind (res1 res2) (,(aoc2024:symb 'day- (format nil "~2,'0d" day)))
                                    (is (= res1 ,expect1))
                                    (is (= res2 ,expect2))))))))

(make-aoc-tests ((1 2264607 19457120)
                 (2 359 418)
                 (3 188192787 113965544)
                 (4 2543 1930)
                 (5 7074 4828)
                 (6 5305 2143)
                 (7 1298300076754 248427118972289)
                 (8 222 884)
                 (9 6279058075753 6301361958738)
                 (10 459 1034)
                 (11 217443 257246536026785)
                 ;(12 0 0)
                 ;(13 0 0)
                 ;(14 0 0)
                 ;(15 0 0)
                 ;(16 0 0)
                 ;(17 0 0)
                 ;(18 0 0)
                 ;(19 0 0)
                 ;(20 0 0)
                 ;(21 0 0)
                 ;(22 0 0)
                 ;(23 0 0)
                 ;(24 0 0)
                 ;(25 0 0)
                 ))
