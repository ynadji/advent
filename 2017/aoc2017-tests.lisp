(in-package :cl-user)
(defpackage test-aoc2017
  (:use #:cl #:aoc2017 #:aoc-utils)
  (:shadowing-import-from #:fiveam
                          #:def-suite
                          #:in-suite
                          #:test
                          #:is)
  (:export #:aoc2017))

(in-package :test-aoc2017)

(def-suite aoc2017)
(in-suite aoc2017)

;; NB: Captures TEST and IS from :FIVEAM. Evaluates DAY multiple times, but we
;; only use it below so it isn't super important. I would rather have this in
;; utils.lisp but it would search for IS/TEST in AOC2017 instead of
;; TEST-AOC2017. Wasn't sure how to fix that. Obviously only makes sense for my
;; inputs d;D
(defmacro make-aoc-tests (test-cases)
  `(progn
     ,@(loop for (day expect1 expect2) in test-cases
             collect `(test ,(symb 'test- day)
                        (time (multiple-value-bind (res1 res2) (,(symb 'day- (format nil "~2,'0d" day)))
                                (is (equal res1 ,expect1))
                                (is (equal res2 ,expect2))))))))

(make-aoc-tests ((1 1343 1274)
                 ;;(2 "36629" "99C3D")
                 ;;(3 1032 1838)
                 ;;(4 409147 991)
                 ;;;;(5 "f97c354d" "863dde27")
                 ;;(6 "wkbvmikb" "evakwaga")
                 ;;(7 118 260)
                 ;;(8 110 "ZJHRKCPLYJ")
                 ;;(9 152851 11797310782)
                 ;;(10 56 7847)
                 ;;;;(11 33 57)
                 ;;(12 318117 9227771)
                 ;;(13 90 135)
                 ;;;;(14 15168 20864)
                 ;;(15 121834 3208099)
                 ;;(16 "10010010110011010" "01010100101011100")
                 ;;(17 "RLDRUDRDDR" 498)
                 ;;(18 2005 20008491)
                 ;;(19 1816277 1410967)
                 ;;(20 14975795 101)
                 ;;(21 "agcebfdh" "afhdbegc")
                 ;;(22 955 246)
                 ;;(23 12000 479008560)
                 ;;(24 460 668)
                 ;;(25 198 -1)
                 ))
