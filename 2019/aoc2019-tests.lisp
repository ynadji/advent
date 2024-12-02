(in-package :cl-user)
(defpackage test-aoc2019
  (:use #:cl #:aoc2019)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is)
  (:export #:aoc2019))

(in-package :test-aoc2019)

(def-suite aoc2019)
(in-suite aoc2019)

;; NB: Captures TEST and IS from :FIVEAM. Evaluates DAY multiple times, but we
;; only use it below so it isn't super important. I would rather have this in
;; utils.lisp but it would search for IS/TEST in AOC2019 instead of
;; TEST-AOC2019. Wasn't sure how to fix that. Obviously only makes sense for my
;; inputs d;D
(defmacro make-aoc-tests (test-cases)
  `(progn
     ,@(loop for (day expect1 expect2) in test-cases
             collect `(test ,(aoc2019:symb 'test- day)
                        (time (multiple-value-bind (res1 res2) (,(aoc2019:symb 'day- (format nil "~2,'0d" day)))
                                (is (= res1 ,expect1))
                                (is (= res2 ,expect2))))))))

(make-aoc-tests ((1 3198599 4795042)
                 ;(2 0 0)
                 ;(3 0 0)
                 ;(4 0 0)
                 ;(5 0 0)
                 ;(6 0 0)
                 ;(7 0 0)
                 ;(8 0 0)
                 ;(9 0 0)
                 ;(10 0 0)
                 ;(11 0 0)
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
