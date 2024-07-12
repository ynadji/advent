(in-package :aoc2023)

(defstruct hand type type-rank card-values)

(defun bag (x) (fset:with (fset:empty-bag) x))

(defun most-common (b)
  (let ((seq nil))
    (fset:do-bag-pairs (x freq b)
      (push (cons x freq) seq))
    (sort seq #'> :key #'cdr)))

(defun card-value (c)
  (let ((digit (digit-char-p c)))
    (if digit
        digit
        (cond ((eql #\T c) 10)
              ((eql #\J c) 11)
              ((eql #\Q c) 12)
              ((eql #\K c) 13)
              ((eql #\A c) 14)))))

(defun calc-hand-type (b)
  (let* (;(size (fset:set-size b))
         (top (most-common b))
         (freqs (mapcar #'cdr top)))
    (cond ((equal freqs '(5))         (values :five-of-a-kind 7))
          ((equal freqs '(4 1))       (values :four-of-a-kind 6))
          ((equal freqs '(3 2))       (values :full-house 5))
          ((equal freqs '(3 1 1))     (values :three-of-a-kind 4))
          ((equal freqs '(2 2 1))     (values :two-pair 3))
          ((equal freqs '(2 1 1 1))   (values :pair 2))
          ((equal freqs '(1 1 1 1 1)) (values :high-card 1)))))

(defun make-hand-from-string (string)
  (let* ((card-values (map 'list #'card-value string))
         (bag (reduce #'fset:bag-sum (mapcar #'bag card-values))))
    (multiple-value-bind (type type-rank) (calc-hand-type bag)
        (make-hand :type type :type-rank type-rank :card-values card-values))))

(defun make-hand-from-card-values (card-values)
  (let ((bag (reduce #'fset:bag-sum (mapcar #'bag card-values))))
    (multiple-value-bind (type type-rank) (calc-hand-type bag)
        (make-hand :type type :type-rank type-rank :card-values card-values))))

(defun compare-hands (h1 h2)
  (if (/= (hand-type-rank h1) (hand-type-rank h2))
      (< (hand-type-rank h1) (hand-type-rank h2))
      (loop for x in (hand-card-values h1)
            for y in (hand-card-values h2)
            do (when (/= x y)
                 (return (< x y))))))

(defun jokerfy-hand (hand)
  (flet ((joker? (c) (= c 11)))
    (if (every #'joker? (hand-card-values hand))
        (make-hand :type :five-of-a-kind :type-rank 7 :card-values (make-list 5 :initial-element 1))
        (let* ((card-values (hand-card-values hand))
               (card-values-no-jokers (remove 11 card-values))
               (bag (->> card-values-no-jokers
                      (mapcar #'bag)
                      (reduce #'fset:bag-sum)))
               (mode-card-value (caar (most-common bag)))
               (new-hand (->> card-values
                           (substitute mode-card-value 11)
                           (make-hand-from-card-values))))
          (setf (hand-card-values new-hand) (substitute 1 11 card-values))
          new-hand))))

(defun read-hands-and-bids (input-file &key (jokerfy? nil))
  (let ((tmp (->> input-file
               (uiop:read-file-lines)
               (mapcar (lambda (s) (str:split " " s :omit-nulls t))))))
    (mapcar #'cons
            (if jokerfy?
                (->> tmp
                  (mapcar #'first)
                  (mapcar #'make-hand-from-string)
                  (mapcar #'jokerfy-hand))
                (->> tmp
                  (mapcar #'first)
                  (mapcar #'make-hand-from-string)))
            (->> tmp
              (mapcar #'second )
              (mapcar #'parse-integer)))))

(defun total-winnings (hands-and-bids)
  (let ((sorted-hands-and-bids (sort hands-and-bids #'compare-hands :key #'first)))
    (loop for (hand . bid) in sorted-hands-and-bids for multiplier from 1
          summing (* bid multiplier))))

(defun day-07-part-1 (input-file)
  (let ((hands-and-bids (read-hands-and-bids input-file)))
    (total-winnings hands-and-bids)))

(defun day-07-part-2 (input-file)
  (let ((hands-and-bids (read-hands-and-bids input-file :jokerfy? t)))
    (total-winnings hands-and-bids)))

(defun day-07 ()
  (let ((f #p"7-input.txt"))
    (values (day-07-part-1 f)
            (day-07-part-2 f))))
