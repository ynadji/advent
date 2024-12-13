test:
	sbcl --eval '(progn (asdf:test-system :aoc2024) (quit))'

time:
	sbcl --eval '(progn (asdf:test-system :aoc2024) (quit))' 2>/dev/null | awk '/TEST/ { split($$3,testday,"-") } /seconds of real time/ { printf "day %2d: %5dms\n", testday[2], $$1*1000; total+=$$1*1000 } END { printf "---------------\ntotal: %6dms\n", total }'
