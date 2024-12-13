(asdf:defsystem aoc2024
  :serial t
  :description "Advent of Code 2024 solutions in Common Lisp"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "alexandria" "function-cache" "serapeum" "drakma" "arrow-macros" "cl-graph" "lparallel" "magicl")
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
  :in-order-to ((test-op (test-op :aoc2024/test))))

(asdf:defsystem :aoc2024/test
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("aoc2024" "fiveam")
  :components ((:file "aoc2024-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (uiop:find-symbol* '#:aoc2024
                                                          '#:test-aoc2024))))
