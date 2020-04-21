// FAILS: not_in_scope3.prg:12:9: Variable `test' not in scope.

{
    // Fails, because was_greater is only present in the if scope, and is used
    // outside.
    i := 0;
    while (i < 3) {
        test := i; // _Declares_ the variable
        i = i + 1;
    }

    if (test > 20) { // Fails: Use of a variable not visible in current scope.
    }
}
