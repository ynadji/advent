(in-package :cl-user)
(defpackage test-aoc2025
  (:use #:cl #:aoc2025)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is)
  (:export #:aoc2025))

(in-package :test-aoc2025)

(def-suite aoc2025)
(in-suite aoc2025)

;; NB: Captures TEST and IS from :FIVEAM. Evaluates DAY multiple times, but we
;; only use it below so it isn't super important. I would rather have this in
;; utils.lisp but it would search for IS/TEST in AOC2025 instead of
;; TEST-AOC2025. Wasn't sure how to fix that. Obviously only makes sense for my
;; inputs d;D
(defmacro make-aoc-tests (test-cases)
  `(progn
     ;; Full GC between days
     #+sbcl (sb-ext:gc :full t)
     ,@(loop for (day expect1 expect2) in test-cases
             collect `(test ,(aoc2025:symb 'test- day)
                            (time (multiple-value-bind (res1 res2) (,(aoc2025:symb 'day- (format nil "~2,'0d" day)))
                                    (is (equal res1 ,expect1))
                                    (is (equal res2 ,expect2))))))))

(make-aoc-tests ((1 1081 6689)
                 (2 19574776074 25912654282)
                 (3 16993 168617068915447)
                 (4 1389 9000)
                 (5 756 355555479253787)
                 (6 5171061464548 10189959087258)
                 (7 1490 3806264447357)
                 (8 153328 6095621910)
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
