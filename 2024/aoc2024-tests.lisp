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
     ;; Full GC between days
     (sb-ext:gc :full t)
     ,@(loop for (day expect1 expect2) in test-cases
             collect `(test ,(aoc2024:symb 'test- day)
                            (time (multiple-value-bind (res1 res2) (,(aoc2024:symb 'day- (format nil "~2,'0d" day)))
                                    (is (equal res1 ,expect1))
                                    (is (equal res2 ,expect2))))))))

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
                 (12 1375476 821372)
                 (13 28887 96979582619758)
                 (14 218295000 6870)
                 (15 1475249 1509724)
                 (16 90440 479)
                 (17 "4,1,7,6,4,1,0,2,7" 164279024971453)
                 (18 298 "52,32")
                 (19 319 692575723305545)
                 (20 1399 994807)
                 (21 242484 294209504640384)
                 (22 14691757043 1831)
                 (23 1337 "aw,fk,gv,hi,hp,ip,jy,kc,lk,og,pj,re,sr")
                 (24 42049478636360 "cph,gws,hgj,nnt,npf,z13,z19,z33")
                 (25 3136 0)))
