(declaim (optimize (speed 3) (safety 0)))
(declaim (inline in-range map->))

(in-package :aoc2023)

(defstruct range
  (dst 0 :type fixnum)
  (src 0 :type fixnum)
  (len 0 :type fixnum))
(defstruct mapper ranges)

(defun in-range (x r)
  (declare (fixnum x)
           (range r))
  (and (>= x (range-src r))
       (<  x (+ (range-src r) (range-len r)))))

(defun map-> (map x)
  (declare (fixnum x)
           (mapper map))
  (let ((dst (loop for r of-type range in (mapper-ranges map) do
    (when (in-range x r)
      (return (+ (range-dst r) (- x (range-src r))))))))
    (if dst dst x)))

(defun parse-seeds (line)
  (declare (string line))
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
    (loop for line of-type string in (rest lines) do
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
  (declare (fixnum seed))
  (let ((n seed))
    (declare (fixnum n))
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
  (declare (fixnum n))
  (labels ((aux (list n acc)
             (if (null list)
                 (reverse acc)
                 (aux (nthcdr n list) n (cons (subseq list 0 (min n (length list))) acc)))))
    (when (> n 0)
      (aux list n nil))))

(defun find-min-location-seed-range (lines)
  (multiple-value-bind (seeds maps) (parse-almanac lines)
    (loop for seed-range in (group seeds 2) minimize
      (loop for seed of-type fixnum from (first seed-range) below (apply #'+ seed-range)
            minimize (chase-map seed maps)))))

;; lol took ~32 minutes without type annotations, but down to ~2.5 minutes with
;; 1.86B seeds. i learned enough and i'm fine with that for now.
(defun day-05-part-2 (input-file)
  (->> input-file
    (uiop:read-file-lines)
    (find-min-location-seed-range)))

;; ideas for speedups:
;; - only try seeds that map into the first set (could be incorrect)
;; - go in reverse?
;; - range split: https://www.reddit.com/r/adventofcode/comments/18b560a/2023_day_5_part_2_cpu_goes_brrr/kc4dbhq/
(defun day-05 ()
  (let ((f #p"05-input.txt"))
    (values (day-05-part-1 f)
            (day-05-part-2 f))))
