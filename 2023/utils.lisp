(in-package :aoc2023)

;; TODO:
;; neighbors function
;; maybe a general DFS BFS?
;; string of 1,2,3 to (1 2 3) for sure
;; see if other functions should be moved here?
;; macro (?) for defining test cases
;; macro (?) for defining final function (define-day-with-input ...) or something? should fetch input in demand.

(defun circular! (items)
  "Modifies the last cdr of list ITEMS, returning a circular list"
  (setf (cdr (last items)) items))

(defun make-one-hot-encoder ()
  (let ((index -1)
        (symbol->index (make-hash-table :test #'equal)))
    #'(lambda (x)
        (if (gethash x symbol->index)
            (nth-value 0 (gethash x symbol->index))
            (progn
              (setf (gethash x symbol->index) (incf index)))))))

(defmacro if-let (bindings &body body)
  "Bind `bindings` in parallel and execute `then` if all are true, or `else` otherwise.

  `body` must be of the form `(...optional-declarations... then else)`.

  This macro combines `if` and `let`.  It takes a list of bindings and
  binds them like `let` before executing the `then` branch of `body`, but
  if any binding's value evaluates to `nil` the process stops there and the
  `else` branch is immediately executed (with no bindings in effect).

  If any `optional-declarations` are included they will only be in effect
  for the `then` branch.

  Examples:

    (if-let ((a (progn (print :a) 1))
             (b (progn (print :b) 2)))
      (list a b)
      'nope)
    ; =>
    :A
    :B
    (1 2)

    (if-let ((a (progn (print :a) nil))
             (b (progn (print :b) 2)))
      (list a b)
      'nope)
    ; =>
    :A
    NOPE

  "
  (alexandria:with-gensyms (outer inner)
    (multiple-value-bind (body declarations) (alexandria:parse-body body)
      (destructuring-bind (then else) body
        `(block ,outer
           (block ,inner
             (let ,(loop :for (symbol value) :in bindings
                         :collect `(,symbol (or ,value
                                                (return-from ,inner nil))))
               ,@declarations
               (return-from ,outer ,then)))
           ,else)))))

;; TODO update docs here
(defmacro if-let (bindings &body body)
  "Bind `bindings` in parallel and execute `then` if all are true, or `else` otherwise.

  `body` must be of the form `(...optional-declarations... then else)`.

  This macro combines `if` and `let`.  It takes a list of bindings and
  binds them like `let` before executing the `then` branch of `body`, but
  if any binding's value evaluates to `nil` the process stops there and the
  `else` branch is immediately executed (with no bindings in effect).

  If any `optional-declarations` are included they will only be in effect
  for the `then` branch.

  Examples:

    (if-let ((a (progn (print :a) 1))
              (b (progn (print :b) 2)))
      (list a b)
      'nope)
    ; =>
    :A
    :B
    (1 2)

    (if-let ((a (progn (print :a) nil))
             (b (progn (print :b) 2)))
      (list a b)
      'nope)
    ; =>
    :A
    NOPE

  "
  (alexandria:with-gensyms (outer inner)
    (multiple-value-bind (body declarations) (alexandria:parse-body body)
      (destructuring-bind (then else) body
        `(block ,outer
           (block ,inner
             (let* ,(loop :for (symbol value) :in bindings
                          :collect `(,symbol (or ,value
                                                 (return-from ,inner nil))))
               ,@declarations
               (return-from ,outer ,then)))
           ,else)))))

(defmacro when-let (bindings &body body)
  "Bind `bindings` in parallel and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let`.  It takes a list of bindings and
  binds them like `let` before executing `body`, but if any binding's value
  evaluates to `nil` the process stops and `nil` is immediately returned.

  Examples:

    (when-let ((a (progn (print :a) 1))
               (b (progn (print :b) 2))
      (list a b))
    ; =>
    :A
    :B
    (1 2)

    (when-let ((a (progn (print :a) nil))
               (b (progn (print :b) 2)))
      (list a b))
    ; =>
    :A
    NIL

  "
  (alexandria:with-gensyms (block)
    `(block ,block
       (let ,(loop :for (symbol value) :in bindings
                   :collect `(,symbol (or ,value
                                          (return-from ,block nil))))
         ,@body))))

;; TODO update docs
(defmacro when-let* (bindings &body body)
  "Bind `bindings` in parallel and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let`.  It takes a list of bindings and
  binds them like `let` before executing `body`, but if any binding's value
  evaluates to `nil` the process stops and `nil` is immediately returned.

  Examples:

    (when-let ((a (progn (print :a) 1))
               (b (progn (print :b) 2))
      (list a b))
    ; =>
    :A
    :B
    (1 2)

    (when-let ((a (progn (print :a) nil))
               (b (progn (print :b) 2)))
      (list a b))
    ; =>
    :A
    NIL

  "
  (alexandria:with-gensyms (block)
    `(block ,block
       (let* ,(loop :for (symbol value) :in bindings
                    :collect `(,symbol (or ,value
                                           (return-from ,block nil))))
         ,@body))))

;; AOC utils
(defun make-cookie-jar (session-cookie)
  (make-instance 'drakma:cookie-jar
                 :cookies (list (make-instance 'drakma:cookie :name "session" :domain "adventofcode.com" :value session-cookie))))

(let ((session-cookie (-> #P"~/.aoc-session-cookie" uiop:read-file-string str:trim)))
  (defun fetch-day-input-file (year day)
    (let ((cached-file (format nil "~a-input.txt" day))
          (url (format nil "https://adventofcode.com/~a/day/~a/input" year day)))
      (if-let ((path (probe-file cached-file)))
        path
        (progn
          (with-open-file (out cached-file :if-does-not-exist :create :direction :output)
            (princ (drakma:http-request url :cookie-jar (make-cookie-jar session-cookie))
                   out))
          (probe-file cached-file))))))
