(in-package :aoc2016)

(defparameter test-input "value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2")

(defun parse-bot-instructions (input-file)
  (loop for line in (uiop:read-file-lines input-file)
        for nums = (string-to-num-list line)
        for fields = (str:split #\Space line)
        if (= 2 (length nums))
          collect nums
        else
          collect (append nums (list (intern (str:upcase (nth 5 fields)) :keyword)
                                     (intern (str:upcase (nth 10 fields)) :keyword)))))

(defun day-10% (input-file)
  (labels ((inspect-chips (instructions bots outputs &optional )
             (loop for inst in instructions for (bot low high low-type high-type) = inst
                   for bot-vals = (gethash bot bots)
                   for low-ht = (if (eq low-type :bot) bots outputs)
                   for high-ht = (if (eq high-type :bot) bots outputs)
                   if (= 2 (length bot-vals))
                     do (push (apply #'min bot-vals) (gethash low low-ht))
                        (push (apply #'max bot-vals) (gethash high high-ht))
                   else
                     collect inst)))
    (let ((instructions (parse-bot-instructions input-file))
          (bots (make-hash-table))
          (outputs (make-hash-table)))
      (loop for inst in instructions
            when (= 2 (length inst))
              do (push (first inst) (gethash (second inst) bots)))
      (let ((instructions (remove-if-not (lambda (xs) (= 5 (length xs))) instructions)))
        (loop while instructions
              do (setf instructions (inspect-chips instructions bots outputs))
              finally (return (values (ax:rassoc-value (ax:hash-table-alist bots) (list 61 17) :test #'equal)
                                      (apply #'* (loop for x upto 2 append (gethash x outputs))))))))))

(defun day-10 ()
  (let ((f (fetch-day-input-file 2016 10)))
    (day-10% f)))
