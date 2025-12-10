(asdf:defsystem aoc2025
  :serial t
  :description "Advent of Code 2025 solutions in Common Lisp"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "alexandria" "function-cache" "serapeum" "drakma" "arrow-macros" "cl-graph" "lparallel" "magicl" "cl-ansi-text" "cl-heap" "btrie" "disjoint-sets" "aoc-utils" "queues" "queues.simple-queue")
  :components ((:file "pkg")
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
               (:file "12"))
  :in-order-to ((test-op (test-op :aoc2025/test))))

(asdf:defsystem :aoc2025/test
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("aoc2025" "fiveam")
  :components ((:file "aoc2025-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (uiop:find-symbol* '#:aoc2025
                                                          '#:test-aoc2025))))
