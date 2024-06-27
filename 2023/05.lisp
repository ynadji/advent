(in-package :aoc2023)

(defstruct range dst src len)
(defstruct mapper ranges)

(defun in-range (x r)
  (and (>= x (range-src r))
       (<  x (+ (range-src r) (range-len r)))))

(defun map-> (map x)
  (let ((dst (loop for r in (mapper-ranges map) do
    (when (in-range x r)
      (return (+ (range-dst r) (- x (range-src r))))))))
    (if dst dst x)))

(defun parse-seeds (line)
  (->> line
    (str:replace-first "seeds: " "")
    (str:split " ")
    (mapcar #'parse-integer)))

(defun parse-almanac-map (line)
  (destructuring-bind (dst-start src-start length) (->> line
                                                     (str:split " ")
                                                     (mapcar #'parse-integer))
    (make-range :dst dst-start :src src-start :len length)))

(defun parse-almanac (lines)
  (let ((maps nil)
        (seeds (parse-seeds (first lines)))
        (map (make-mapper)))
    (loop for line in (rest lines) do
      (cond ((str:ends-with? "map:" line)
             (unless (null (mapper-ranges map))
               (push map maps)
               (setf map (make-mapper))))
            ((str:empty? line)) ; do nothing. this is just so i don't have to check for "" before aref
            ((digit-char-p (aref line 0))
             (push (parse-almanac-map line) (mapper-ranges map))))
          finally (push map maps))
    (values seeds (reverse maps))))

(defun chase-map (seed maps)
  (let ((n seed))
   (loop for m in maps do
     (setq n (map-> m n))
         finally (return n))))

(defun find-min-location (lines)
  (multiple-value-bind (seeds maps) (parse-almanac lines)
    (loop for seed in seeds minimize (chase-map seed maps))))

(defun day-05-part-1 (input-file)
  (->> input-file
    (uiop:read-file-lines)
    (find-min-location)))

(defun group (list n)
  (labels ((aux (list n acc)
             (if (null list)
                 (reverse acc)
                 (aux (nthcdr n list) n (cons (subseq list 0 (min n (length list))) acc)))))
    (when (> n 0)
      (aux list n nil))))

(defun find-min-location-seed-range (lines)
  (multiple-value-bind (seeds maps) (parse-almanac lines)
    (loop for seed-range in (group seeds 2) minimize
      (loop for seed from (first seed-range) below (apply #'+ seed-range)
            minimize (chase-map seed maps)))))

;; lol took ~32 minutes
(defun day-05-part-2 (input-file)
  (->> input-file
    (uiop:read-file-lines)
    (find-min-location-seed-range)))

;; ideas:
;; - only try seeds that map into the first set (could be incorrect)
;; - go in reverse
