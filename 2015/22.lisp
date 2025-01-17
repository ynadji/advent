(in-package :aoc2015)

(defparameter *debug* nil)
(defparameter *hard-mode* nil)

(defstruct (unit (:print-function print-unit)) name (hp 0) (mana 0) (armor 0) (boss-atk 8)) ;; make output simpler
(defstruct effect (damage 0) (mana 0) (armor 0))
(defstruct spell name (timer 1 :type fixnum) mana effect targets)

(defun print-unit (unit stream depth)
  (declare (ignore depth))
  (format stream "    ~a: ~a/~a (~a)" (unit-name unit) (unit-hp unit) (unit-mana unit) (unit-armor unit)))

(defparameter *available-spells*
  (list (make-spell :name 'magic-missile :mana 53 :effect (make-effect :damage 4) :targets 'boss)
        (make-spell :name 'drain :mana 73 :effect (make-effect :damage 2) :targets 'both)
        (make-spell :name 'shield :mana 113 :timer 6 :effect (make-effect :armor 7) :targets 'player)
        (make-spell :name 'poison :mana 173 :timer 6 :effect (make-effect :damage 3) :targets 'boss)
        (make-spell :name 'recharge :mana 229 :timer 5 :effect (make-effect :mana 101) :targets 'player)))

(defun cast! (spell player boss)
  (if (member (spell-name spell) '(magic-missile drain attack))
      (format *debug* "~a casts ~a on ~a~%" (if (eq (spell-name spell) 'attack) 'boss 'player) (spell-name spell) (spell-targets spell))
      (format *debug* "~a procs on ~a~%" (spell-name spell) (spell-targets spell)))
  (labels ((modify (target spell &optional invert-dmg)
             (format *debug* "~a" target)
             (let ((effect (spell-effect spell)))
               (when (plusp (effect-damage effect))
                 (decf (unit-hp target) (if invert-dmg
                                            (- (effect-damage effect))
                                            (max 1 (- (effect-damage effect)
                                                      (unit-armor target))))))
               (when (plusp (effect-mana effect))
                 (incf (unit-mana target) (effect-mana effect)))
               (when (and (plusp (effect-armor effect))
                          (zerop (unit-armor target)))
                 (incf (unit-armor target) (effect-armor effect)))
               (format *debug* " -> ~a~%" target))))
    (ecase (spell-targets spell)
      (boss (modify boss spell))
      (player (modify player spell))
      (both (modify player spell t) (modify boss spell)))
    (dolist (unit (list boss player))
      (when (<= (unit-hp unit) 0)
        (format *debug* "~a has died!~%" (unit-name unit))
        (throw 'game-over (list player boss nil))))))

;; function for "choose possible spells" hmm how can i make this work while bruteforcing?
;;; this is the point where i need to be able to backtrack if it fails. this
;;; seems messy with what i have so far...
(defun fight (player boss spell-names)
  (labels ((find-spell (name spells)
             (find-if (lambda (s) (eq name (spell-name s))) spells))
           (proc-active-spells (active-spells)
             (loop for dot in active-spells
                   do (cast! dot player boss)
                      (decf (spell-timer dot))
                      (when (zerop (spell-timer dot))
                        (format *debug* "~a effect has worn off~%" (spell-name dot)))
                      (when (and (eq 'shield (spell-name dot))
                                 (zerop (spell-timer dot)))
                        (decf (unit-armor player) (effect-armor (spell-effect dot))))
                   when (plusp (spell-timer dot))
                     collect dot)))
    (let ((boss-atk (make-spell :name 'attack :mana 0 :effect (make-effect :damage (unit-boss-atk boss)) :targets 'player))
          (hard-mode (make-spell :name 'hard-mode :mana 0 :effect (make-effect :damage 1) :targets 'player))
          active-spells)
      (catch 'game-over
        (loop while (and (plusp (unit-hp player)) (plusp (unit-hp boss)))
              for n from 1
              for sname in spell-names
              for spell = (copy-spell (find-spell sname *available-spells*))
              do (format *debug* "# TURN ~a~%" n)
              when *hard-mode*
                do (cast! hard-mode player boss)
              do (format *debug* "## PLAYER TURN~%")
              do (setf active-spells (proc-active-spells active-spells))
              do (if (> (spell-timer spell) 1)
                     (progn (when (find-spell (spell-name spell) active-spells)
                              (throw 'game-over (list player boss 'double-cast)))
                            (format *debug* "~a begins casting ~a on ~a~%" 'player (spell-name spell) (spell-targets spell))
                            (push spell active-spells))
                     (cast! spell player boss))
                 (decf (unit-mana player) (spell-mana spell))
                 (when (minusp (unit-mana player))
                   (throw 'game-over (list player boss 'out-of-mana)))
              do (format *debug* "## BOSS TURN~%")
              do (setf active-spells (proc-active-spells active-spells))
              do (cast! boss-atk player boss)
                 (when *debug* (terpri)))))))

(defun won? (&optional player boss early-fail?)
  (and player boss
       (not early-fail?)
       (plusp (unit-hp player))
       (not (plusp (unit-hp boss)))))

(defun unwinnable? (&optional player boss early-fail?)
  (and player boss
       (or early-fail? (not (plusp (unit-hp player))))))

(defun find-win% (player boss &optional (spell-sequences '(nil)) (available-spells '((magic-missile) (drain) (shield) (poison) (recharge))))
  (labels ((new-spell-sequences (spell-sequences available-spells)
             (loop for to-try in available-spells
                   append (loop for spell-sequence in spell-sequences
                                collect (append spell-sequence to-try)))))
    (loop for op = (copy-unit player) for ob = (copy-unit boss)
          for spell-sequence in (new-spell-sequences spell-sequences available-spells)
          for res = (fight op ob spell-sequence)
          when (apply #'won? res)
            collect spell-sequence into wins
          unless (apply #'unwinnable? res)
            collect spell-sequence into possibilities
          finally
             (return (values possibilities wins)))))

(defun compute-mana-cost (spells)
  (reduce #'+ (mapcar #'spell-mana (mapcar (lambda (spell-name) (find spell-name *available-spells* :key #'spell-name)) spells))))

(defun find-win (player boss &optional (spell-sequences '(nil)))
  (multiple-value-bind (possible-spell-sequences winning-spell-sequences)
      (find-win% player boss spell-sequences)
    (if winning-spell-sequences
        (apply #'min (mapcar #'compute-mana-cost winning-spell-sequences))
        (find-win player boss possible-spell-sequences))))

;; this just tries every possible combination and takes forever (47s!). you
;; really need to both clean up the code and figure out ways to prune this so it
;; runs more quickly.
;;
;; probably worth making the test cases run with just a function so you can
;; ensure you don't break those while refactoring.
(defun day-22-part-1 (input-file)
  (destructuring-bind (boss-hp boss-atk) (string-to-num-list (uiop:read-file-string input-file))
    (find-win (make-unit :name 'player :hp 50 :mana 500 :armor 0)
              (make-unit :name 'boss :hp boss-hp :boss-atk boss-atk))))

(defun day-22-part-2 (input-file)
  (let ((*hard-mode* t))
    (destructuring-bind (boss-hp boss-atk) (string-to-num-list (uiop:read-file-string input-file))
      (find-win (make-unit :name 'player :hp 50 :mana 500 :armor 0)
                (make-unit :name 'boss :hp boss-hp :boss-atk boss-atk)))))

(defun day-22 ()
  (let ((f (fetch-day-input-file 2015 22)))
    (values (day-22-part-1 f)
            (day-22-part-2 f))))
