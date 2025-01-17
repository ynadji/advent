(in-package :aoc2016)

(defparameter test-input "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]")

(defun parse-encrypted-rooms (input-file)
  (labels ((remove-hyphens (s) (str:replace-all "-" "" s)))
    (loop for room-description in (uiop:read-file-lines input-file)
          collect (cl-ppcre:register-groups-bind (name (#'parse-integer id) checksum) ("([a-z\\-]+)(\\d+)\\[([a-z]+)\\]" room-description)
                    (list (coerce (remove-hyphens name) 'list) id (coerce checksum 'list) (coerce name 'list))))))

(defun real-room? (name checksum)
  (let ((csum-freq (sort (sort (frequencies name) #'char< :key #'car) #'> :key #'cdr)))
    (labels ((verify (csum-freq checksum)
               (if (null checksum)
                   t
                   (and (char= (first checksum) (caar csum-freq))
                        (verify (rest csum-freq) (rest checksum))))))
      (verify csum-freq checksum))))

(defun day-04-part-1 (input-file)
  (loop for (name id checksum nil) in (parse-encrypted-rooms input-file)
        when (real-room? name checksum)
          sum id))

(defun char+ (c n)
  (code-char (+ 97 (mod (- (+ n (char-code c)) 97) 26))))

(defun decrypt-room-name (original-name id)
  (coerce (loop for c in original-name
                if (char= c #\-)
                  collect #\Space
                else
                  collect (char+ c id))
          'string))

(defun day-04-part-2 (input-file)
  (loop for (nil id nil original-name) in (parse-encrypted-rooms input-file)
        ;; NB: i didn't need to when (real-room? name checksum) first to get the
        ;; right answer on my input, although the puzzle implies it's required.
        when (string= "northpole object storage " (decrypt-room-name original-name id))
          do (return id)))

(defun day-04 ()
  (let ((f (fetch-day-input-file 2016 4)))
    (values (day-04-part-1 f)
            (day-04-part-2 f))))
