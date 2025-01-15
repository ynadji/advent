(in-package :aoc2015)
(defparameter *debug* t)

(defstruct (unit (:print-function print-unit)) name (hp 0) (mana 0) (armor 0)) ;; make output simpler
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
      ;; TODO: other lose conditions (no mana, uhh i think that's it)
      (when (<= (unit-hp unit) 0)
        (format *debug* "~a has died!~%" (unit-name unit))
        (throw 'game-over unit)))))

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
    (let ((boss-atk (make-spell :name 'attack :effect (make-effect :damage 8) :targets 'player))
          active-spells
          (mana-spent 0)) ; won't bubble up through the transfer of control
      (catch 'game-over
        (loop while (and (plusp (unit-hp player)) (plusp (unit-hp boss)))
              for n from 1
              for sname in spell-names
              for spell = (find-spell sname *available-spells*)
              do (format *debug* "# TURN ~a~%" n)
              do (format *debug* "## PLAYER TURN~%")
              do (setf active-spells (proc-active-spells active-spells))
              do (if (> (spell-timer spell) 1)
                     (unless (find-spell (spell-name spell) active-spells)
                       (format *debug* "~a begins casting ~a on ~a~%" 'player (spell-name spell) (spell-targets spell))
                       (push spell active-spells))
                     (cast! spell player boss))
                 (decf (unit-mana player) (spell-mana spell))
                 (incf mana-spent (spell-mana spell))
              do (format *debug* "## BOSS TURN~%")
              do (setf active-spells (proc-active-spells active-spells))
              do (cast! boss-atk player boss)
                 (terpri))))))

(defun day-22-part-1 (input-file) (progn input-file -1))

(defun day-22-part-2 (input-file) (progn input-file -1))

(defun day-22 ()
  (let ((f (fetch-day-input-file 2015 22)))
    (values (day-22-part-1 f)
            (day-22-part-2 f))))
