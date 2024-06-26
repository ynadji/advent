(asdf:defsystem aoc2023
  :serial t
  :description "Advent of Code 2023 solutions in Common Lisp"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "arrow-macros")
  :components ((:file "01")
               (:file "02")
               (:file "03"))
  :in-order-to ((test-op (test-op :aoc2023/tests))))

(asdf:defsystem "aoc2023/tests"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("aoc2023" "fiveam")
  :components ((:file "aoc2023-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :aoc2023)))
