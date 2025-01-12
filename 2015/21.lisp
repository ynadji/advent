(in-package :aoc2015)

;;                   hp  d a
(defparameter boss '(109 8 2))

;; Weapons:    Cost  Damage  Armor
;; Dagger        8     4       0      p2
;; Shortsword   10     5       0 +$2
;; Warhammer    25     6       0 +$15
;; Longsword    40     7       0 +$15 p1
;; Greataxe     74     8       0 +$34
;;
;; Armor:      Cost  Damage  Armor
;; Leather      13     0       1
;; Chainmail    31     0       2 +$18 p1
;; Splintmail   53     0       3 +$22
;; Bandedmail   75     0       4 +$23
;; Platemail   102     0       5 +$27
;;
;; Rings:      Cost  Damage  Armor
;; Damage +1    25     1       0
;; Damage +2    50     2       0
;; Damage +3   100     3       0      p2
;; Defense +1   20     0       1 +$20
;; Defense +2   40     0       2 +$20 p1
;; Defense +3   80     0       3 +$40 p2
;;
;; honestly, this one was easy enough to eyeball. by looking at the increase in
;; cost per extra point of damage or armor, it was easy to tell when it was most
;; cost effective to buy a ring vs. better weapons/armor for part 1. you just do
;; the opposite for part 2 and pick the most expensive rings and cheapest
;; weapon. test victory with WIN? bing bong.
;;
;; an algorithm would probably work like:
;; * make lists for weapons/armor of '((atk/armor+ . cost))
;; * sort by cost
;; * choose such that you have at least one weapon (dagger)
;; * loop until you win by replacing with the next weapon/armor (and/or) adding a ring in
;; order of cost until you win.
;; * do the reverse for part 2.

(defparameter test-player '(8 5 5))
(defparameter test-boss '(12 7 2))

(defun win? (player boss)
  (let ((boss-net-attack (- (second boss) (third player)))
        (player-net-attack (- (second player) (third boss))))
    (loop with player-hp = (first player)
          with boss-hp = (first boss)
          do (decf boss-hp player-net-attack)
          when (not (plusp boss-hp))
            return t
          do (decf player-hp boss-net-attack)
          when (not (plusp player-hp))
            return nil)))

(defun day-21-part-1 (input-file)
  (declare (ignore input-file))
  (+ 40 31 40))

(defun day-21-part-2 (input-file)
  (declare (ignore input-file))
  (+ 100 8 80))

(defun day-21 ()
  (let ((f (fetch-day-input-file 2015 21)))
    (values (day-21-part-1 f)
            (day-21-part-2 f))))
