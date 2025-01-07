(in-package :aoc2015)

(defparameter test-input "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.")

(defun parse-potential-happiness (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        with happy-map and all-names
        do (cl-ppcre:register-groups-bind ((#'sintern name-1 op) (#'parse-integer x) (#'sintern name-2))
               ("(\\w+) would (lose|gain) (\\d+) happiness units by sitting next to (\\w+)\\." line)
             (push (cons (cons name-1 name-2)
                         (ecase op (|gain| x) (|lose| (- x))))
                   happy-map)
             (push name-1 all-names)
             (push name-2 all-names))
        finally (return (values happy-map
                                (remove-duplicates all-names)))))

(defun score-arrangement (arrangement happy-map)
  (labels ((score-direction (list)
             (+ (loop for (n1 n2) on list
                      when n2
                        sum (ax:assoc-value happy-map (cons n1 n2) :test #'equal))
                (ax:assoc-value happy-map (cons (first list) (ax:lastcar list)) :test #'equal))))
    (+ (score-direction arrangement)
       (score-direction (reverse arrangement)))))

;; TODO: It's a bit slow and you could reduce the amount of double work you do.
(defun day-13-part-1 (input-file)
  (multiple-value-bind (happy-map all-names) (parse-potential-happiness input-file)
    (let ((max-happiness most-negative-fixnum))
      (ax:map-permutations (lambda (arrangement) (ax:maxf max-happiness
                                                     (score-arrangement arrangement happy-map)))
                           all-names)
      max-happiness)))

(defun day-13-part-2 (input-file)
  (multiple-value-bind (happy-map all-names) (parse-potential-happiness input-file)
    (let ((max-happiness most-negative-fixnum)
          (all-names (cons '|Yacin| all-names))
          (happy-map (append happy-map (loop for name in all-names
                                             append (list (cons (cons '|Yacin| name) 0)
                                                          (cons (cons name '|Yacin|) 0))))))
      (ax:map-permutations (lambda (arrangement) (ax:maxf max-happiness
                                                     (score-arrangement arrangement happy-map)))
                           all-names)
      max-happiness)))

(defun day-13 ()
  (let ((f (fetch-day-input-file 2015 13)))
    (values (day-13-part-1 f)
            (day-13-part-2 f))))
