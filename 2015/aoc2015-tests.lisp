(in-package :cl-user)
(defpackage test-aoc2015
  (:use #:cl #:aoc2015)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is)
  (:export #:aoc2015))

(in-package :test-aoc2015)

(def-suite aoc2015)
(in-suite aoc2015)

;; NB: Captures TEST and IS from :FIVEAM. Evaluates DAY multiple times, but we
;; only use it below so it isn't super important. I would rather have this in
;; utils.lisp but it would search for IS/TEST in AOC2015 instead of
;; TEST-AOC2015. Wasn't sure how to fix that. Obviously only makes sense for my
;; inputs d;D
(defmacro make-aoc-tests (test-cases)
  `(progn
     ,@(loop for (day expect1 expect2) in test-cases
             collect `(test ,(aoc2015:symb 'test- day)
                        (time (multiple-value-bind (res1 res2) (,(aoc2015:symb 'day- (format nil "~2,'0d" day)))
                                (is (equal res1 ,expect1))
                                (is (equal res2 ,expect2))))))))

(make-aoc-tests ((1 74 1795)
                 (2 1586300 3737498)
                 (3 2592 2360)
                 (4 282749 9962624)
                 (5 238 69)
                 (6 377891 14110788)
                 (7 956 40149)
                 (8 1333 2046)
                 (9 141 736)
                 (10 329356 4666278)
                 (11 "cqjxxyzz" "cqkaabcc")
                 (12 111754 65402)
                 (13 733 725)
                 (14 2655 1059)
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
