(asdf:defsystem aoc-utils
  :serial t
  :description "Advent of Code Utilities"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "alexandria" "arrow-macros" "function-cache" "drakma" "cl-ansi-text" "cl-heap")
  :components ((:file "utils")
               (:file "paip-search")
               (:file "export-all"))
  :in-order-to ((test-op (test-op :aoc-utils/test))))
