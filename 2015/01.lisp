(loop for c across (uiop:read-file-string #P"../2015/1-input.txt") with floor = 0 if (char= c #\() do (incf floor) else do (decf floor) finally (return floor))

(loop for i from 1 for c across (uiop:read-file-string #P"../2015/1-input.txt") with floor = 0 if (char= c #\() do (incf floor) else do (decf floor) when (= floor -1) return i)
