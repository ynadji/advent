(in-package :aoc2023)

(defun day-18-part-1 (input-file) (progn input-file -1))

(defun day-18-part-2 (input-file) (progn input-file -1))

(defun day-18 ()
  (let ((f (fetch-day-input-file 2023 18)))
    (values (day-18-part-1 f)
            (day-18-part-2 f))))

































































(defun day18 (is-part-two)
  (loop :with moves
          := (loop :for line :in (uiop:read-file-lines #P"./18-input.txt")
                   :collect (ppcre:register-groups-bind (d1 n1 n2 d2)
                                ("^(.) (\\d+) \\(#(.....)(.)\\)" line)
                              (if is-part-two
                                  (list (case (parse-integer d2 :radix 16)
                                          (0 #\R)
                                          (1 #\D)
                                          (2 #\L)
                                          (3 #\U))
                                        (parse-integer n2 :radix 16))
                                  (list (char d1 0) (parse-integer n1)))))
        :and current := '(0 0)
        :for (d n) :in moves :for (dx dy) := (case d
                                               (#\L `(,(- n) 0))
                                               (#\R `(,n 0))
                                               (#\U `(0 ,(- n)))
                                               (#\D `(0 ,n)))
        :for (x1 y1) := current
        :for x2 := (+ x1 dx) :for y2 := (+ y1 dy)
        :sum (* (+ y1 y2) (- x1 x2)) :into sum :sum n :into total-moves
        :do (setf current (list x2 y2))
        :finally (return (+ (/ (abs sum) 2) (1+ (/ total-moves 2))))))
