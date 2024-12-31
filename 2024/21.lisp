(in-package :aoc2024)

(declaim (optimize debug))

(defparameter *init-pos-keypad* '(3 . 2))
(defparameter *keypad* '((:7 :8 :9) (:4 :5 :6) (:1 :2 :3) (:U :0 :A)))
(defparameter *key->pos* (loop for i from 0 for row in *keypad* append (loop for j from 0 for col in row collect (cons col (cons i j)))))

(defparameter *init-pos-arrows* '(0 . 2))
(defparameter *arrows* '((:U :^ :A) (:< :|v| :>)))
(defparameter *arrow->pos* (loop for i from 0 for row in *arrows* append (loop for j from 0 for col in row collect (cons col (cons i j)))))

(defparameter test-input "029A
980A
179A
456A
379A")

(defparameter test-output "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A")

(defun pos-compare (comp part &rest points)
  (apply comp (mapcar part points)))

(defun pos<=x (&rest points) (apply #'pos-compare #'<= #'car points))
(defun pos>=x (&rest points) (apply #'pos-compare #'>= #'car points))
(defun pos<=y (&rest points) (apply #'pos-compare #'<= #'cdr points))
(defun pos>=y (&rest points) (apply #'pos-compare #'>= #'cdr points))

(defun shortest-path-botato (pos1 pos2 undef)
  (with-output-to-string (stream)
    (destructuring-bind (row-diff . col-diff)
        (pos- pos2 pos1)
      (when (and (not (equal undef (pos+ pos1 (cons 0 col-diff))))
                 (minusp col-diff))
        (dotimes (_ (abs col-diff))
          (write-char #\< stream)))
      (when (not (equal undef (pos+ pos1 (cons row-diff 0))))
        (dotimes (_ (abs row-diff))
          (if (minusp row-diff)
              (write-char #\^ stream)
              (write-char #\v stream))))
      (when (plusp col-diff)
        (dotimes (_ col-diff)
          (write-char #\> stream)))
      (when (equal undef (pos+ pos1 (cons row-diff 0)))
        (dotimes (_ (abs row-diff))
          (if (minusp row-diff)
              (write-char #\^ stream)
              (write-char #\v stream))))
      (when (and (equal undef (pos+ pos1 (cons 0 col-diff)))
                 (minusp col-diff))
        (dotimes (_ (abs col-diff))
          (write-char #\< stream))))))

(defun shortest-path (pos1 pos2 &optional (undef '(0 . 0)))
  (let* ((p-tmp (copy-list pos1))
         panic?
         (route (with-output-to-string (s)
                  (destructuring-bind (row-diff . col-diff) (pos- pos2 pos1)
                    (when (minusp col-diff)
                      (incf (cdr p-tmp) col-diff)
                      (loop repeat (abs col-diff) do (write-char #\< s)))
                    (when (minusp row-diff)
                      (incf (car p-tmp) row-diff)
                      (loop repeat (abs row-diff) do (write-char #\^ s)))
                    (when (plusp row-diff)
                      (incf (car p-tmp) row-diff)
                      (loop repeat row-diff do (write-char #\v s)))
                    (when (plusp col-diff)
                      (incf (cdr p-tmp) col-diff)
                      (loop repeat col-diff do (write-char #\> s)))
                    (when (and (= (car undef) (car p-tmp))
                               (or (pos<=y pos1 p-tmp undef)
                                   (pos>=y pos1 p-tmp undef)))
                      (setf panic? t)
                      ;;(format t "panic y! ~a ~a ~a~%" pos1 p-tmp undef)
                      )
                    (when (and (= (cdr undef) (cdr p-tmp))
                               (or (pos<=x pos1 p-tmp undef)
                                   (pos>=x pos1 p-tmp undef)))
                      (setf panic? t)
                      ;;(format t "panic x! ~a ~a ~a~%" pos1 p-tmp undef)
                      )
                    ))))
    (if panic?
        (reverse route)
        (values route (reverse route)))))

(defun char->keyword (c)
  (intern (format nil "~a" c) :keyword))

(defun path= (&rest paths)
  (cond ((null paths) t)
        ((null (rest paths)) t)
        (t (labels ((char-sort (s) (sort s #'char<)))
             (and (every #'string=
                         (mapcar #'char-sort (str:split "A" (first paths)))
                         (mapcar #'char-sort (str:split "A" (second paths))))
                  (path= (rest (rest paths))))))))

(defun shortest-paths% (code pad)
  (let ((code-keywords (map 'list #'char->keyword (str:concat "A" code)))
        (undef (ax:assoc-value pad :U)))
    (str:concat (str:join "A"
                          (loop for (k1 k2) on code-keywords
                                while k2
                                for pos1 = (ax:assoc-value pad k1)
                                for pos2 = (ax:assoc-value pad k2)
                                ;;do (format t "~a -> ~a ~a ~a~%" (ax:rassoc-value pad pos1) (ax:rassoc-value pad pos2) pos1 pos2)
                                collect (shortest-path-botato pos1 pos2 undef)))
                "A")))

(function-cache:defcached do-dpad (path n)
  (loop repeat n
        do (setf path (shortest-paths% path *arrow->pos*))
        finally (return path)))

(defun shortest-paths (code &optional (n 2))
  (do-dpad (shortest-paths% code *key->pos*) n))

(defun day-21-part-1 (input-file)
  (let ((codes (uiop:read-file-lines input-file)))
    (loop for code in codes
          for sp = (shortest-paths code)
          ;;do (format t "~a: ~a~%" code (shortest-paths code))
          ;;do (format t "(* ~a ~a)~%" (length sp) (parse-integer code :junk-allowed t))
          sum (* (length sp) (parse-integer code :junk-allowed t)))))

(defun day-21-part-2 (input-file)
  (let ((codes (uiop:read-file-lines input-file)))
    (loop for code in codes
          for sp = (shortest-paths code 10)
          for all-sp = (loop for partial in (cl-ppcre:all-matches-as-strings ".*?A" sp) sum (length (do-dpad partial 15)))
          ;;for all-sp = (str:concat (str:join "A" partials) "A")
          ;;do (format t "~a: ~a~%" code (shortest-paths code 25))
          ;;do (format t "(* ~a ~a)~%" (length sp) (parse-integer code :junk-allowed t))
          sum (* all-sp (parse-integer code :junk-allowed t)))))

(defun day-21 ()
  (let ((f (fetch-day-input-file 2024 21)))
    (values (day-21-part-1 f)
            (day-21-part-2 f))))
