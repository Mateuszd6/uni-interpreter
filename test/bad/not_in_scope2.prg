// FAILS: not_in_scope2.prg:9:9: Variable `test' not in scope.

{
    // Fails, because was_greater is only present in the if scope, and is used
    // outside.
    for (i : 10 .. 12)
        test := i; // _Declares_ the variable

    if (test > 20) // Fails: Use of a variable not visible in current scope.
    {
    }
}
