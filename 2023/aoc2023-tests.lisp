(ql:quickload :fiveam)
;;(use-package '(:fiveam))

(5am:def-suite aoc2023)
(5am:in-suite aoc2023)

(5am:test test-day-01
  (multiple-value-bind (res1 res2) (day-01)
    (5am:is (= res1 55447))
    (5am:is (= res2 54706))))

(5am:test test-day-02
  (multiple-value-bind (res1 res2) (day-02)
    (5am:is (= res1 2795))
    (5am:is (= res2 75561))))

(5am:test test-day-03
  (multiple-value-bind (res1 res2) (day-03)
    (5am:is (= res1 551094))
    (5am:is (= res2 80179647))))
