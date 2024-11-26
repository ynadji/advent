(asdf:defsystem aoc2023
  :serial t
  :description "Advent of Code 2023 solutions in Common Lisp"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "arrow-macros" "fset" "alexandria" "function-cache" "split-sequence" "drakma" "cl-graph" "queues.priority-queue")
  :components ((:file "pkg")
               (:file "utils")
               (:file "01")
               (:file "02")
               (:file "03")
               (:file "04")
               (:file "05")
               (:file "06")
               (:file "07")
               (:file "08")
               (:file "09")
               (:file "10")
               (:file "11")
               (:file "12")
               (:file "13")
               (:file "14")
               (:file "15")
               (:file "16")
               (:file "17")
               (:file "18")
               (:file "19")
               (:file "20")
               (:file "21")
               (:file "22")
               (:file "23")
               (:file "24")
               (:file "25"))
  :in-order-to ((test-op (test-op :aoc2023/test))))

(asdf:defsystem :aoc2023/test
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("aoc2023" "fiveam")
  :components ((:file "aoc2023-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (uiop:find-symbol* '#:aoc2023
                                                          '#:test-aoc2023))))
