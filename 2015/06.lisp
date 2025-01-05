(defun do-lights (lights op i1 j1 i2 j2)
  (loop for i from i1 upto i2 do
        (loop for j from j1 upto j2 do
              (ecase op
                (|on| (setf (aref lights i j) 1))
                (|off| (setf (aref lights i j) 0))
                (|toggle| (setf (aref lights i j)
                                (logxor (aref lights i j) 1)))))))

(defun count-lights (lights)
  (reduce #'+ (make-array 1000000 :element-type 'bit :displaced-to lights)))

(defun day-6-part-1 (input-file)
  (let ((lights (make-array '(1000 1000) :element-type 'bit)))
    (loop for line in (uiop:read-file-lines input-file)
          do (cl-ppcre:register-groups-bind ((#'intern op) (#'parse-integer i1 j1 i2 j2)) ("(on|off|toggle).*?(\\d+),(\\d+).*?(\\d+),(\\d+)" line)
               (do-lights lights op i1 j1 i2 j2)))
    (count-lights lights)))

(defun count-brightness (lights)
  (reduce #'+ (make-array 1000000 :displaced-to lights)))

(defun do-lights2 (lights op i1 j1 i2 j2)
  (loop for i from i1 upto i2 do
        (loop for j from j1 upto j2 do
              (ecase op
                (|on| (incf (aref lights i j)))
                (|off| (setf (aref lights i j)
                             (max 0 (1- (aref lights i j)))))
                (|toggle| (incf (aref lights i j) 2))))))

(defun day-6-part-2 (input-file)
  (let ((lights (make-array '(1000 1000))))
    (loop for line in (uiop:read-file-lines input-file)
          do (cl-ppcre:register-groups-bind ((#'intern op) (#'parse-integer i1 j1 i2 j2)) ("(on|off|toggle).*?(\\d+),(\\d+).*?(\\d+),(\\d+)" line)
               (do-lights2 lights op i1 j1 i2 j2)))
    (count-brightness lights)))
