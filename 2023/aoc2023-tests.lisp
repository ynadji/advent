(in-package :cl-user)
(defpackage test-aoc2023
  (:use #:cl)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals
                #:def-fixture
                #:with-fixture)
  (:export #:aoc2023))

(in-package :test-aoc2023)

(def-suite aoc2023)
(in-suite aoc2023)

(test test-day-01
  (time (multiple-value-bind (res1 res2) (aoc2023:day-01)
          (is (= res1 55447))
          (is (= res2 54706)))))

(test test-day-02
  (time (multiple-value-bind (res1 res2) (aoc2023:day-02)
          (is (= res1 2795))
          (is (= res2 75561)))))

(test test-day-03
  (time (multiple-value-bind (res1 res2) (aoc2023:day-03)
          (is (= res1 551094))
          (is (= res2 80179647)))))

(test test-day-04
  (time (multiple-value-bind (res1 res2) (aoc2023:day-04)
          (is (= res1 21213))
          (is (= res2 8549735)))))

(test test-day-05
  (time (multiple-value-bind (res1 res2) (aoc2023:day-05)
          (is (= res1 650599855))
          (is (= res2 1240035)))))

(test test-day-06
  (time (multiple-value-bind (res1 res2) (aoc2023:day-06)
          (is (= res1 1159152))
          (is (= res2 41513103)))))

(test test-day-07
  (time (multiple-value-bind (res1 res2) (aoc2023:day-07)
          (is (= res1 249726565))
          (is (= res2 251135960)))))

(test test-day-08
  (time (multiple-value-bind (res1 res2) (aoc2023:day-08)
          (is (= res1 16697))
          (is (= res2 10668805667831)))))
