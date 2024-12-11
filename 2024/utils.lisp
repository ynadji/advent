(in-package :aoc2024)

;; TODO:
;; maybe a general DFS BFS?
;; string of 1,2,3 to (1 2 3) for sure
;; see if other functions should be moved here?
;; export all of these!
;; maybe just use http://quickutil.org/how ?

(defun group (list n)
  (declare (fixnum n))
  (labels ((aux (list n acc)
             (if (null list)
                 (reverse acc)
                 (aux (nthcdr n list) n (cons (subseq list 0 (min n (length list))) acc)))))
    (when (> n 0)
      (aux list n nil))))

;; let over lambda and on lisp stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                    collect (symb 'a i))
       ,(funcall
         (get-macro-character #\`) stream nil)))

  (set-dispatch-macro-character #\# #\` #'|#`-reader|))

(defmacro dis (args &rest body)
  `(disassemble
    (compile nil
             (lambda ,(mapcar (lambda (a)
                                (if (consp a)
                                    (cadr a)
                                    a))
                       args)
               (declare
                ,@(mapcar
                   #`(type ,(car a1) ,(cadr a1))
                   (remove-if-not #'consp args)))
               ,@body))))

(defun circular! (items)
  "Modifies the last cdr of list ITEMS, returning a circular list"
  (setf (cdr (last items)) items))

(defun make-label-encoder ()
  "Returns a function that given any object X differentiable by EQUAL, will return
a unique identifier that maps X to a unique, increasing integer."
  (let ((index -1)
        (symbol->index (make-hash-table :test #'equal)))
    #'(lambda (x)
        (if (gethash x symbol->index)
            (nth-value 0 (gethash x symbol->index))
            (progn
              (setf (gethash x symbol->index) (incf index)))))))

;; AOC utils
(defun make-cookie-jar (session-cookie)
  (make-instance 'drakma:cookie-jar
                 :cookies (list (make-instance 'drakma:cookie :name "session"
                                                              :domain "adventofcode.com"
                                                              :value session-cookie))))

(let ((session-cookie (-> #P"~/.aoc-session-cookie" uiop:read-file-string str:trim)))
  (defun fetch-day-input-file (year day)
    (let ((cached-file (format nil "~a-input.txt" day))
          (url (format nil "https://adventofcode.com/~a/day/~a/input" year day)))
      (ax:if-let ((path (probe-file cached-file)))
        path
        (progn
          (with-open-file (out cached-file :if-does-not-exist :create :direction :output)
            (princ (drakma:http-request url :cookie-jar (make-cookie-jar session-cookie))
                   out))
          (probe-file cached-file))))))

(defvar *day-template* "(in-package :aoc~a)

(defun day-~2,'0d-part-1 (input-file) (progn input-file -1))

(defun day-~2,'0d-part-2 (input-file) (progn input-file -1))

(defun day-~2,'0d ()
  (let ((f (fetch-day-input-file ~a ~a)))
    (values (day-~2,'0d-part-1 f)
            (day-~2,'0d-part-2 f))))
")

(defun define-aoc-day (year day)
  (format nil *day-template* year day day day year day day day))

(defun make-aoc-project (year)
  (loop for day from 1 upto 25 do
    (let ((fname (format nil "~2,'0d.lisp" day)))
      (when (-> fname probe-file not)
        (with-open-file (out fname :direction :output :if-does-not-exist :create)
          (princ (define-aoc-day year day) out)))))
  ;; to add:
  ;; * aocYEAR.asd template
  ;; * pkg.list template
  ;; * aocYEAR-tests.lisp template. by default have all but day 1 commented out
  ;; so you don't hammer the site when you run tests.
  )

;; AOC puzzle utils
(defmacro with-input ((var string) &body body)
  `(uiop:with-temporary-file (:pathname ,var)
     (str:to-file ,var ,string)
     ,@body))

(defun print-input (input)
  "Prints input from puzzles after UIOP:READ-FILE-LINES."
  (dolist (x input) (princ x) (terpri)))

;; TODO: Generalize the above to be more like PRINT-MAZE from 10.lisp so parts
;; can be printed in color. Maybe a separate PRINT-ARRAY is needed where colors
;; can be based on (1) MEMBER of POS or (2) MEMBER of (AREF i j).

(defun string-to-chars (string) (coerce string 'list))
(defun chars-to-string (chars) (coerce chars 'string))

(defun transpose-strings (lines)
  "Transposes a list of strings."
  (->> lines
    (mapcar #'string-to-chars)
    (apply #'mapcar 'list)
    (mapcar #'chars-to-string)))

(defun transpose (lines)
  (cond ((null lines) nil)
        ((listp (first lines)) (apply #'mapcar 'list lines))
        ((stringp (first lines)) (transpose-strings lines))))

(defun rotate-90-clockwise (rows)
  "Rotate ROWS of reversable things (like STRINGs or SEQUENCEs)"
  (->> rows transpose (mapcar #'reverse)))

(defun rotate-270-clockwise (rows)
  (-> rows rotate-90-clockwise rotate-90-clockwise rotate-90-clockwise))

(defun zip (&rest lists) (transpose lists))
(defun unzip (list-of-lists) (transpose list-of-lists))

(defun zip-pairs (list1 list2) (mapcar #'cons list1 list2))
(defun unzip-pairs (list-of-cons)
  (loop for (x . y) in list-of-cons
        collect x into cars
        collect y into cdrs
        finally (return (values cars cdrs))))

(defun flatten-once (list)
  (loop for x in list
        if (listp x)
          append x
        else
          collect x))

(defun interleave (&rest lists)
  (flatten-once (apply #'zip lists)))

;; TODO: You could dispatch between UNZIP and UNZIP-PAIRS by seeing if the first
;; list-of-X is a CONS or a LIST.

;;;; Grid functions
(defvar *cardinals* '(:north :east :south :west))
(defvar *cardinals-pos-delta* '((-1 . 0) (0 . 1) (1 . 0) (0 . -1)))
(defvar *inter-cardinals* '(:north-east :south-east :south-west :north-west))
(defvar *inter-cardinals-pos-delta* '((-1 . 1) (1 . 1) (1 . -1) (-1 . -1)))
(defvar *8-winds* (interleave *cardinals* *inter-cardinals*))
(defvar *8-winds-pos-delta* (interleave *cardinals-pos-delta* *inter-cardinals-pos-delta*))
(defvar *8-winds/deltas* (mapcar #'cons *8-winds* *8-winds-pos-delta*))

(defun opposite-direction (direction)
  (nth (mod (+ 4 (position direction *8-winds*)) 8) *8-winds*))

(defun 90-clockwise-direction (direction)
  (nth (mod (+ 2 (position direction *8-winds*)) 8) *8-winds*))

(defstruct 2d-index (x 0 :type fixnum) (y 0 :type fixnum))

;; seems much slower than POS+ given the output from DISASSEMBLE
(defun idx+ (p1 p2)
  ;;(declare (optimize (speed 3) (safety 0)))
  (make-2d-index :x (+ (2d-index-x p1) (2d-index-x p2))
                 :y (+ (2d-index-y p1) (2d-index-y p2))))

(defun pos+ (p1 p2)
  (declare (optimize (speed 3) (safety 0)))
  (cons (the fixnum (+ (the fixnum (car p1)) (the fixnum (car p2))))
        (the fixnum (+ (the fixnum (cdr p1)) (the fixnum (cdr p2))))))

;; how can i make it so i can easily do pos or i,j as argument?
;; this will likely be a bottleneck. how can i make it faster?
;; i can probably define a compiler macro for this: https://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm
;;
;; since we typically know the values of WANTED-DIRECTIONS at run-time. also uhh
;; why are using *8-winds... et al. instead of just the WANTED-DIRECTIONS?
(defun 2d-neighbors (M pos &key (reachable? (lambda (M pos dir) (declare (ignorable M pos dir)) t))
                             (wanted-directions *cardinals*))
  (declare (type (simple-array standard-char (* *)) M)
           (type (compiled-function) reachable?)
           (optimize (speed 3) (safety 0)))
  (let ((maxrow (array-dimension M 0))
        (maxcol (array-dimension M 1))
        (all-indices (mapcar (lambda (p) (pos+ pos p)) *8-winds-pos-delta*)))
    (declare (fixnum maxrow maxcol))
    (let (valid-indices valid-directions)
      ;; NB: you use i, j notation for matrices, but incorrectly use x, y here.
      ;; i < numrows is y, j is < numcols is x. Might be worth switching to (x, y)
      ;; notation since it's more natural.
      (loop for (x . y) in all-indices for direction in *8-winds*
            when (and (and (>= (the fixnum x) 0) (< x maxrow))
                      (and (>= (the fixnum y) 0) (< y maxcol))
                      (and (member direction wanted-directions))
                      (and (funcall reachable? M (cons x y) direction)))
              do (push (cons x y) valid-indices)
                 (push direction valid-directions))
      (values valid-indices
              valid-directions))))

;; TODO: define a WALK function that operates with 2D-NEIGHBORS. By default,
;; tracks visited nodes and doesn't go back. should prob have DFS vs. BFS
;; options. should really just look at the PAIP functions for tree walks,
;; searching, etc. seems like at a minimum you need:
;; * successors function
;; * tracking the path thus far
;; * everything that 2d-neighbors has

;; TODO: integrate Prolog somehow?
;; what if i just embedded swipl? https://www.swi-prolog.org/pldoc/man?section=embedded
(defun num-digits (x)
  (declare (type fixnum x))
  (1+ (floor (log x 10))))
