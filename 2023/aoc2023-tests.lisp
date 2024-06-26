(use-package '(:fiveam))

(def-suite aoc2023)
(in-suite aoc2023)

(test test-day-01
  (multiple-value-bind (res1 res2) (day-01)
    (is (= res1 55447))
    (is (= res2 54706))))

(test test-day-02
  (multiple-value-bind (res1 res2) (day-02)
    (is (= res1 2795))
    (is (= res2 75561))))

(test test-day-03
  (multiple-value-bind (res1 res2) (day-03)
    (is (= res1 551094))
    (is (= res2 80179647))))
