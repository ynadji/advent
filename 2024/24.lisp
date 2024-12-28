(in-package :aoc2024)

(declaim (optimize (debug 3)))

(defparameter test-input-1 "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02")

(defparameter test-input-2 "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj")

(defun read-initial-numbers (input-file)
  (let ((x 0) (y 0))
   (destructuring-bind (state-string ops) (str:split (format nil "~%~%") (uiop:read-file-string input-file))
     (declare (ignore ops))
     (loop for line in (str:split #\Newline state-string) do
       (cl-ppcre:register-groups-bind (prefix (#'parse-integer bit-index) (#'parse-integer val)) ("(x|y)(\\d+): (\\d)" line)
         ;;(format t "~a~a ~a~%" prefix bit-index val)
         (if (string= prefix "x")
             (incf x (* val (expt 2 bit-index)))
             (incf y (* val (expt 2 bit-index))))))
     (values (format nil "~b" (+ x y))
             x y (+ x y)))))

(defun incorrect-bits (correct &optional (current (format nil "~b" 42049478636360)) (stop-at-first? t))
  (format t "~a~%" correct)
  (format t "~a~%" current)
  (loop for x across (reverse correct) for y across (reverse current) for i from 0
        unless (char= x y)
          do (format t "z~2,'0d differs. had ~a wanted ~a~%" i y x)
             (when stop-at-first?
               (return-from incorrect-bits (intern (format nil "z~2,'0d" i))))))

(defun read-gates (input-file)
  (let ((states (make-hash-table))
        z-states)
    (destructuring-bind (state-string ops) (str:split (format nil "~%~%") (uiop:read-file-string input-file))
      (loop for line in (str:split #\Newline state-string) do
        (cl-ppcre:register-groups-bind ((#'symb wire) (#'parse-integer val)) ("(\\w+): (\\d)" line)
          (setf (gethash wire states) val)))
      (values states
              (loop for line in (str:split #\Newline ops)
                    when (string/= "" line)
                      collect (cl-ppcre:register-groups-bind ((#'intern s1 op s2 s3)) ("(\\w+) (\\w+) (\\w+) -> (\\w+)" line)
                                (when (char= #\z (char (format nil "~a" s3) 0))
                                  (push s3 z-states))
                                (list s1 op s2 s3)))
              (sort z-states #'string< :key (lambda (x) (format nil "~a" x)))))))

;;(defparameter oper->fun (list 'AND #'logand 'OR #'logior 'XOR #'logxor))
;;(print (list 'AND #'logand 'OR #'logior 'XOR #'logxor))
;;
;; for some reason having XOR there causes a problem when i run it from SBCL
;; but not from SLIME? it complains:
;; 
(defun oper->fun (op) (case op (AND #'logand) (OR #'logior) (t #'logxor)))

(defun all-z-states-computed? (states z-states)
  (loop for z in z-states always (gethash z states)))

(defun combine-zs (states z-states)
  (when (all-z-states-computed? states z-states)
    (loop for n from 0 for state in z-states
          for val = (gethash state states)
          while val
          sum (* val (expt 2 n)))))

(defparameter *swaps* '((|gws| . |nnt|)
                        (|z33| . |hgj|)
                        (|z19| . |cph|)
                        (|z13| . |npf|)))

(defun swap (sym)
  (let ((new-sym sym))
    (loop for (x . y) in *swaps*
          when (eq sym x)
            do (setf new-sym y)
          when (eq sym y)
            do (setf new-sym x))
    (unless (eq sym new-sym)
      ;;(format t "replacing ~a with ~a~%" sym new-sym)
      )
    new-sym))

(defun run-ops (states ops)
  (loop for (s1 op s2 s3-orig) in ops
        ;;for s3 = (swap s3-orig)
        for s3 = s3-orig
        ;;do (format t "~a ~a ~a ~a funcall ~a~%" s1 op s2 s3 (oper->fun op))
        if (and (gethash s1 states) (gethash s2 states))
          do (setf (gethash s3 states) (funcall (oper->fun op)
                                                (gethash s1 states)
                                                (gethash s2 states)))
        else
          when op
            collect (list s1 op s2 s3)))

(defun day-24-part-1 (input-file)
  (multiple-value-bind (states ops z-states) (read-gates input-file)
    ;;(print ops)
    (let ((remaining-ops ops))
      (loop do (setf remaining-ops (run-ops states remaining-ops))
            ;;do (format t "~a remaining states~%" remaining-ops)
            while remaining-ops
            finally
               (return (combine-zs states z-states))))))

(defun test-manual-numbers (input-file &optional (x (random (expt 2 45))) (y (random (expt 2 45))))
  (multiple-value-bind (states ops z-states) (read-gates input-file)
    (let ((correct (format nil "~46,'0b" (+ x y))))
      (loop for i from 0
            for x across (reverse (format nil "~46,'0b" x))
            for y across (reverse (format nil "~46,'0b" y))
            for x-symb = (intern (format nil "x~2,'0d" i))
            for y-symb = (intern (format nil "y~2,'0d" i))
            do ;;(format t "setting ~a to ~a and ~a to ~a~%" x-symb x y-symb y)
               (setf (gethash x-symb states) (digit-char-p x)
                     (gethash y-symb states) (digit-char-p y)))
      (let ((remaining-ops ops))
        (loop do (setf remaining-ops (run-ops states remaining-ops))
                 ;;do (format t "~a remaining states~%" remaining-ops)
              while remaining-ops
              finally
                 (progn ;;(format t "~a: ~a~%~a: ~a~%" correct (+ x y) (format nil "~46,'0b" (combine-zs states z-states)) (combine-zs states z-states))
                        (return (values (incorrect-bits correct (format nil "~46,'0b" (combine-zs states z-states)))))))))))

(defun day-24-part-2 (input-file)
  (declare (ignore input-file))
  (str:join "," (sort (mapcar (lambda (x) (format nil "~a" x)) (ax:flatten *swaps*)) #'string<)))

(defun day-24 ()
  (let ((f (fetch-day-input-file 2024 24)))
    (values (day-24-part-1 f)
            (day-24-part-2 f))))
