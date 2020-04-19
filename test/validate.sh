#!/bin/bash
# This script does require bash, I'm sorry...

# Kill old failed_tests directory:
rm -rf ./failed_tests/
errors=0

for f in ./bad/*.prg; do
    echo -n "Testing" $f
    # TODO: make sure stdout is empty and error msges are pritned to stderr
    ../interpreter < $f &> ./test.err
    diff > temp.diff                                                           \
      <(cat $f | grep -e "^// FAILS: " | head -n 1 | sed "s/^\/\/ FAILS: //g") \
      <(cat ./test.err | tail -n 1)
    if [ $? -ne 0 ]; then
        mkdir -p failed_tests
        mkdir -p failed_tests/bad
        mkdir -p failed_tests/good

        mv ./temp.diff "./failed_tests/$f"
        echo " FAILED! (diff saved: failed_tests/$f)";
        errors=$((errors + 1))
    else
        echo " OK"
    fi
done

if [ $errors -eq 0 ]; then
    echo ""
    echo "All tests OK! I guess it's time to do something else now."
    echo "Maybe programming in a language that doesn't cause an instant headache..."
else
    echo ""
    echo "$errors failed tests!"
fi

# Cleanup:
rm -rf ./test.err
rm -rf ./test.out
