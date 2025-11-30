# Bigger heap is mostly to help counteract the GC during timing runs.
test:
	sbcl --dynamic-space-size 8192 --eval '(progn (asdf:test-system :aoc2025) (quit))'

time:
	sbcl --dynamic-space-size 8192 --eval '(progn (asdf:test-system :aoc2025) (quit))' 2>/dev/null | awk '/TEST/ { split($$3,testday,"-") } /seconds of real time/ { printf "day %2d: %5dms\n", testday[2], $$1*1000; total+=$$1*1000 } END { printf "---------------\ntotal: %6dms\n", total }'

time-ros:
	ros run -- --dynamic-space-size 8192 --eval '(progn (asdf:test-system :aoc2025) (quit))' 2>/dev/null | awk '/TEST/ { split($$3,testday,"-") } /seconds of real time/ { printf "day %2d: %5dms\n", testday[2], $$1*1000; total+=$$1*1000 } END { printf "---------------\ntotal: %6dms\n", total }'
