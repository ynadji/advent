#!/bin/bash
cat input.txt | tr -d '[a-zA-Z]' | sed -E 's/^(.).*(.)$/\1\2/' | sed -E 's/^(.)$/\1\1/' | awk '{ sum+=$1 } END { print sum; }'

# This is hacky. We specifically grep for numbers that share letters, then
# replace those digits with their surrounding shared letters around the digit
# so we don't prematurely consume a character needed by the next spelled out
# digit.
cat input.txt | sed 's/$/$/' | ggrep -P '(twone|oneight|threeight|fiveight|nineight|eighthree|eightwo|sevenine|one|two|three|four|five|six|seven|eight|nine|\d+|\$)' -o | sed 's/one/o1e/g' | sed 's/two/t2o/g' | sed 's/three/t3e/g' | sed 's/four/4/g' | sed 's/five/5e/g' | sed 's/six/6/g' | sed 's/seven/7n/g' | sed 's/eight/e8t/g' | sed 's/nine/n9e/g' | tr -d '\n[a-z]' | tr '$' '\n' | sed -E 's/^(.).*(.)$/\1\2/' | sed -E 's/^(.)$/\1\1/' | awk '{ sum+=$1 } END { print sum; }'
