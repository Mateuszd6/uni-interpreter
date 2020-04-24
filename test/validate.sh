#!/bin/bash
# This script does require bash, I'm sorry...

# Kill old failed_tests directory:
rm -rf ./failed_tests/
errors=0

for i in 'good' 'bad'; do
    echo "Running suite: '$i':"
    cd $i
    for f in *.prg; do
        echo -n "  Testing" $f
        # TODO: make sure stdout is empty and error msges are pritned to stderr
        ../../interpreter $f > /dev/null 2> ../test.err
        EXIT_CODE=$?
        diff > ../temp.diff                                                           \
             <(cat $f | grep -e "^// FAILS: " | head -n 1 | sed "s/^\/\/ FAILS: //g") \
             <(cat ../test.err)
        DIFF_RET=$?

        EXPECTED_RET=0
        cat $f | head -n 1 | grep -qe "^// FAILS: "
        if [ $? -eq 0 ]; then
            EXPECTED_RET=1 # Expect the test to fail.
        fi

        if [ $EXIT_CODE -ne $EXPECTED_RET ] || [ $DIFF_RET -ne 0 ]; then
            mkdir -p ../failed_tests

            mv ../temp.diff "../failed_tests/bad__${f%.prg}.diff"
            echo " FAILED! (diff saved: failed_tests/bad__${f%.prg}.diff)";
            errors=$((errors + 1))
        else
            echo " OK"
        fi
    done
    cd ..
done

if [ $errors -eq 0 ]; then
    echo ""
    echo "All tests OK! I guess it's time to do something else now."
else
    echo ""
    echo "$errors failed tests!"
fi

# Cleanup:
rm -rf ./temp.diff
rm -rf ./test.err
rm -rf ./test.out
