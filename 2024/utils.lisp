(in-package :aoc2024)

;; TODO:
;; maybe a general DFS BFS?
;; string of 1,2,3 to (1 2 3) for sure
;; see if other functions should be moved here?
;; export all of these!
;; maybe just use http://quickutil.org/how ?

(declaim (inline manhattan-distance pos+ pos-))

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

(defun make-pos-color-alist (color positions)
  (loop for pos in positions collect (cons pos color)))

(defun print-grid (grid &key (stream t) char-color-alist pos-color-alist)
  "Print GRID to STREAM. Use ANSI colors based on CHAR-COLOR-ALIST and
POS-COLOR-ALIST. While CHAR-COLOR-ALIST implies it's for characters only,
anything that is EQL inside the GRID will work (i.e., integers)."
  (labels ((index-cols-as-row (max-j start end)
             (str:join "" (mapcar (lambda (s) (subseq s start end))
                                  (loop for j below max-j collect (format nil "~3,' d" j))))))
   (let ((max-j (array-dimension grid 1)))
     (format stream "    ~a~%" (index-cols-as-row max-j 0 1))
     (format stream "    ~a~%" (index-cols-as-row max-j 1 2))
     (format stream "    ~a~%" (index-cols-as-row max-j 2 3))))
  (loop for i below (array-dimension grid 0) do
    (format stream "~3,' d " i)
    (loop for j below (array-dimension grid 1)
          for pos = (cons i j)
          for c = (aref grid i j)
          for color = (or (ax:assoc-value char-color-alist c)
                          (ax:assoc-value pos-color-alist pos :test #'equal))
          do (let ((cl-ansi-text:*ENABLED* color))
               (cl-ansi-text:with-color (color :stream stream)
                 (princ c stream))))
    (format stream "~&")))

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

(defun direction->delta (direction)
  (ax:assoc-value *8-winds/deltas* direction))

(defun directions->deltas (&rest directions)
  (loop for direction in directions collect (cons direction (direction->delta direction))))

(defvar *cardinals/deltas* (apply #'directions->deltas *cardinals*))
(defvar *inter-cardinals/deltas* (apply #'directions->deltas *inter-cardinals*))

(defun advance (direction pos)
  (pos+ pos (direction->delta direction)))

(defun safe-advance (direction pos array)
  (declare (optimize (speed 3)))
  (let ((new-pos (pos+ pos (direction->delta direction))))
    (and (array-in-bounds-p array (car new-pos) (cdr new-pos))
         new-pos)))

(defun peek (direction pos array)
  (let ((new-pos (safe-advance direction pos array)))
    (when new-pos
      (values (aref array (car new-pos) (cdr new-pos))
              new-pos))))

(defun paref (array pos)
  (aref array (car pos) (cdr pos)))

(defun (setf paref) (new-value array pos)
  (setf (aref array (car pos) (cdr pos)) new-value))

(defun peek-to-boundary (direction pos array &optional stop-chars)
  (loop for (c new-pos) = (multiple-value-list (peek direction pos array))
        while c
        collect c into chars collect new-pos into positions
        do (setf pos new-pos)
        while (or (null stop-chars) (not (member c stop-chars)))
        finally (return (values chars positions))))

(defun opposite-direction (direction)
  (nth (mod (+ 4 (position direction *8-winds*)) 8) *8-winds*))

(defun 90-clockwise-direction (direction)
  (nth (mod (+ 2 (position direction *8-winds*)) 8) *8-winds*))

(defun make-inter-cardinal (cardinal-1 cardinal-2)
  (ecase cardinal-1
    ((:north :south) (ecase cardinal-2
                       ((:north :south) nil)
                       ((:east :west) (intern (format nil "~a-~a" cardinal-1 cardinal-2) 'keyword))))
    ((:east :west) (ecase cardinal-2
                     ((:north :south) (make-inter-cardinal cardinal-2 cardinal-1))
                     ((:east :west) nil)))))

(defstruct 2d-index (x 0 :type fixnum) (y 0 :type fixnum))

;; seems much slower than POS+ given the output from DISASSEMBLE
(defun idx+ (p1 p2)
  ;;(declare (optimize (speed 3) (safety 0)))
  (make-2d-index :x (+ (2d-index-x p1) (2d-index-x p2))
                 :y (+ (2d-index-y p1) (2d-index-y p2))))

;; TODO: try declaring these inline to see how much faster it makes
;; it!
(defun pos+ (p1 p2)
  (declare (optimize (speed 3) (safety 0)))
  (cons (the fixnum (+ (the fixnum (car p1)) (the fixnum (car p2))))
        (the fixnum (+ (the fixnum (cdr p1)) (the fixnum (cdr p2))))))

(defun pos- (p1 p2)
  (declare (optimize (speed 3) (safety 0)))
  (cons (the fixnum (- (the fixnum (car p1)) (the fixnum (car p2))))
        (the fixnum (- (the fixnum (cdr p1)) (the fixnum (cdr p2))))))

;; how can i make it so i can easily do pos or i,j as argument?
;; this will likely be a bottleneck. how can i make it faster?
;; i can probably define a compiler macro for this: https://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm
;;
;; since we typically know the values of WANTED-DIRECTIONS at run-time. also uhh
;; why are using *8-winds... et al. instead of just the WANTED-DIRECTIONS?
(defun 2d-neighbors (M pos &key (reachable? (lambda (M pos dir) (declare (ignorable M pos dir)) t))
                             (wanted-directions *cardinals/deltas*))
  (declare (type (compiled-function) reachable?)
           (optimize (speed 3)))
  (let (valid-indices valid-directions)
    ;; NB: you use i, j notation for matrices, but incorrectly use x, y here.
    ;; i < numrows is y, j is < numcols is x. Might be worth switching to (x, y)
    ;; notation since it's more natural.
    (loop for (direction . delta-pos) in wanted-directions
          for new-pos = (pos+ pos delta-pos)
          for x fixnum = (car new-pos) for y fixnum = (cdr new-pos)
          when (and (array-in-bounds-p M x y)
                    (and (funcall reachable? M new-pos direction)))
            do (push new-pos valid-indices)
               (push direction valid-directions))
    (values valid-indices
            valid-directions)))

;; TODO: define a WALK function that operates with 2D-NEIGHBORS. By default,
;; tracks visited nodes and doesn't go back. should prob have DFS vs. BFS
;; options. should really just look at the PAIP functions for tree walks,
;; searching, etc. seems like at a minimum you need:
;; * successors function
;; * tracking the path thus far
;; * everything that 2d-neighbors has

;; TODO: integrate Prolog somehow?
;; what if i just embedded swipl? https://www.swi-prolog.org/pldoc/man?section=embedded

;; this breaks for numbers as low as 9999999
;; AOC2024> (num-digits 999999)
;; 6
;; AOC2024> (num-digits 9999999)
;; 8
;;
;; manually checking in a big COND statement is faster and doesn't have the
;; above problem, but is annoying to write. macro below does it for us.
(defun num-digits% (x)
  (declare (type fixnum x))
  (1+ (floor (log x 10))))

(eval-when (:compile-toplevel)
  (defun make-num-digits-upto% (max-digits)
    (append (cons 'cond
                  (loop for n from 0 below max-digits
                        collect `((< x ,(expt 10 (1+ n))) ,(1+ n))))
            '((t (error "~a too large for NUM-DIGITS. Bump definition value in utils to MAKE-NUM-DIGITS-UPTO." x))))))

(defmacro make-num-digits-upto (max-digits)
  `(defun num-digits (x)
     (declare (type fixnum x)
              (optimize (speed 3)))
     ,(make-num-digits-upto% max-digits)))

(make-num-digits-upto 20)

;; maybe starts makes more sense as just an item to do an EQL or EQUAL
;; comparison to the item? so far it's only FIXNUMs and CHARs.
(defun read-grid (input-file &key (element-type 'standard-char) (starts? (lambda (x) (declare (ignorable x)) nil)))
  (let* ((lines (uiop:read-file-lines input-file))
         (rows (length lines))
         (cols (length (first lines)))
         (grid (make-array (list rows cols) :element-type element-type))
         starts)
    (loop for row in lines for i from 0 do
      (loop for c across row for j from 0
            for x = (ecase element-type
                      (fixnum (digit-char-p c))
                      (standard-char c))
            do
               (setf (aref grid i j) x)
               (when (funcall starts? x)
                 (push (cons i j) starts))))
    (values grid starts)))

(defun parse-grid (string &key (element-type 'standard-char) (starts? (lambda (x) (declare (ignorable x)) nil)))
  (with-input (input-file string)
    (read-grid input-file :element-type element-type :starts? starts?)))

;; From https://github.com/bo-tato
(defun string-to-num-list (string)
  "Return a list of all numbers in STRING."
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "[-\\d]+" string)))

(defun function-size-in-bytes (fun)
  (reduce #'+ (sb-disassem::get-fun-segments fun) :key #'sb-disassem::seg-length))

(defun frequencies (list)
  (let ((ht (make-hash-table)))
    (loop for x in list do (incf (gethash x ht 0)))
    (ax:hash-table-alist ht)))

(defun manhattan-distance (p1 p2)
  (declare (optimize (speed 3)))
  (destructuring-bind (x . y) (pos- p1 p2)
    (declare (type fixnum x y))
    (the fixnum (+ (the fixnum (abs x)) (the fixnum (abs y))))))
