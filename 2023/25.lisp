(in-package :aoc2023)

;; improvements:
;;
;; not actually using a full graph implementation
;; karger-stein
;; copy graph instead of reloading each time
;; something without string splitting/equal. maybe read things in as keywords?

(defun parse-graph-line (s) (str:split "[ :]" s :regex t :omit-nulls t))

(defun make-graph (input-file)
  (let ((g (g:make-graph 'g:graph-container :vertex-test #'equal))
        (lines (->> input-file uiop:read-file-lines (mapcar #'parse-graph-line))))
    (dolist (vertices lines)
      (let ((u (first vertices)))
        (dolist (v (rest vertices))
          (g:add-edge-between-vertexes g u v))))
    g))

(defun copy-graph (g)
  (let ((gnew (g:make-graph 'g:graph-container :vertex-test #'equal)))
    (dolist (v (g:vertexes g))
      (g:add-vertex gnew v))
    (dolist (e (g:edges g))
      (g:add-edge gnew e :force-new? t))
    gnew))

(defun random-edge (g) (nth (-> g g:edges length random) (g:edges g)))

(defun combine-element (u v)
  (str:join "," (sort (append (->> u g:element (str:split ","))
                              (->> v g:element (str:split ",")))
                      #'string<)))

(defun collapse-edge (g e)
  (let* ((vertices (-<>> e g:vertexes (sort <> #'string< :key #'g:element)))
         (u (first vertices))
         (v (second vertices))
         (uv-elt (combine-element u v))
         (uv (g:add-vertex g uv-elt)))
    (g:iterate-neighbors u (lambda (x) (g:add-edge-between-vertexes g uv x :if-duplicate-do :force)))
    (g:iterate-neighbors v (lambda (x) (g:add-edge-between-vertexes g uv x :if-duplicate-do :force)))
    (g:delete-vertex g u)
    (g:delete-vertex g v)
    (loop while (g:find-edge-between-vertexes g uv uv)
          do (g:delete-edge-between-vertexes g uv uv)))
  g)

(defun find-component-size (e)
  (destructuring-bind (v1 v2) (g:vertexes e)
    (* (->> v1 g:element (str:split ",") length)
       (->> v2 g:element (str:split ",") length))))

(defun collapse-random-edge (g)
  (collapse-edge g (random-edge g)))

(defun min-cut-size (g)
  (loop while (> (-> g g:vertexes length) 2)
        do (collapse-random-edge g)
        finally (return (values (length (g:edges g)) g))))

(defun min-cut-size-n-times (input-file n)
  (declare (fixnum n))
  (loop repeat n minimize (min-cut-size (make-graph input-file))))

(defun min-cut-size-with-known-n (input-file n)
  (loop for g = (make-graph input-file)
        do (multiple-value-bind (cut-n g!) (min-cut-size g)
             (declare (fixnum n cut-n))
             (when (= n cut-n)
               (return (-> g! g:edges first find-component-size))))))
