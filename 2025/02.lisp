(in-package :aoc2025)

(defparameter test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(defun parse-id-ranges (input-file)
  (mapcar (lambda (x) (mapcar #'parse-integer (str:split #\- x))) (str:split #\, (uiop:read-file-string input-file))))

(defun invalid-id-p1? (id)
  (let ((num-digits-id (num-digits id)))
    (when (evenp num-digits-id)
      (zerop (mod id (1+ (expt 10 (/ (num-digits id) 2))))))))

(defun one-ohs (length)
  (let ((one-ohs (loop for x from length downto 1 by 2 sum (expt 10 (1- x)))))
    (if (zerop (mod one-ohs 10))
        (/ one-ohs 10)
        one-ohs)))

(defun invalid-id-p2? (id)
  (let ((num-digits-id (num-digits id)))
    (when (> num-digits-id 1)
     (or (invalid-id-p1? id) ;; 10*1
         ;; 101...
         (when (and (zerop (mod num-digits-id 2)) (> num-digits-id 3))
           (zerop (mod id (one-ohs num-digits-id))))
         (when (= num-digits-id 9)
           (zerop (mod id 1001001)))
         ;; 111...
         (zerop (mod id (/ (1- (expt 10 num-digits-id)) 9)))))))

(defun day-02% (input-file &optional (invalid-id? #'invalid-id-p1?))
  (let ((id-ranges (parse-id-ranges input-file)))
    (loop for (start end) in id-ranges
          sum (loop for id from start upto end
                    when (funcall invalid-id? id)
                      sum id))))

(defun day-02 ()
  (let ((f (fetch-day-input-file 2025 2)))
    (values (day-02% f)
            (day-02% f #'invalid-id-p2?))))
