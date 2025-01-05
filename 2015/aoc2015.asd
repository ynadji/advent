(asdf:defsystem aoc2015
  :serial t
  :description "Advent of Code 2015 solutions in Common Lisp"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "alexandria" "function-cache" "serapeum" "drakma" "arrow-macros" "cl-graph" "lparallel" "magicl" "cl-ansi-text" "cl-heap" "btrie" "md5")
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
  :in-order-to ((test-op (test-op :aoc2015/test))))

(asdf:defsystem :aoc2015/test
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("aoc2015" "fiveam")
  :components ((:file "aoc2015-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (uiop:find-symbol* '#:aoc2015
                                                          '#:test-aoc2015))))
