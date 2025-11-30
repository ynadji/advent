(asdf:defsystem aoc-utils
  :serial t
  :description "Advent of Code Utilities"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "alexandria" "function-cache" "serapeum" "drakma" "arrow-macros" "cl-ansi-text" "cl-heap")
  :components ((:file "utils"))
  :in-order-to ((test-op (test-op :aoc-utils/test))))
