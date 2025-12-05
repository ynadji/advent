(in-package :aoc2025)

(defparameter test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(defun parse-id-ranges (input-file)
  (mapcar (lambda (x) (mapcar #'parse-integer (str:split #\- x)))
          (str:split #\, (uiop:read-file-string input-file))))

(defun invalid-id-p1? (id)
  (let ((num-digits-id (num-digits id)))
    (when (evenp num-digits-id)
      (zerop (mod id (1+ (expt 10 (/ (num-digits id) 2))))))))

(defun generate-invalids ()
  (flet ((repeat-int (s k)
           (loop repeat (1- s) with repeated = k with length = (num-digits k)
                 do (setf repeated (+ (* repeated (expt 10 length)) k))
                 finally (return repeated))))
    (let (invalids)
      (loop for n from 1 to 99999
            do (let* ((length (num-digits n)))
                 (when (<= 1 length 5)
                   ;; k from 2 up to floor(10/len), inclusive
                   (loop for k from 2 to (floor 10 length)
                         for s = (repeat-int k n)
                         do (push s invalids)))))
      (remove-duplicates invalids))))

(defun range-check-day-02 (input-file invalids)
  (declare (optimize speed))
  (let ((id-ranges (parse-id-ranges input-file)))
    (loop for invalid fixnum in invalids
          when (loop for (start end) in id-ranges
                       thereis (<= (the fixnum start) invalid (the fixnum end)))
            sum invalid into p2 fixnum and
          when (invalid-id-p1? invalid)
            sum invalid into p1 fixnum
          finally (return (values p1 p2)))))

(defun day-02 ()
  (let ((f (fetch-day-input-file 2025 2))
        (invalids (generate-invalids)))
    (range-check-day-02 f invalids)))
