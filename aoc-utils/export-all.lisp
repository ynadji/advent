(in-package :aoc-utils)

(let ((pack (find-package *package*)))
  (do-all-symbols (sym pack)
    (when (eql (symbol-package sym) pack)
      (export sym))))
