// FAILS: not_in_scope1.prg:36:14: Variable `was_greater' not in scope.

get_int :: () -> int {
    return 3;
}

use_bool :: (b : bool) {
    _ = b; // TODO: Bring it back?
}

{
    // This is correct, write to the variable from outside of the scope.
    was_greater := false;
    if (get_int() > 1) {
        was_greater = true;
    }

    use_bool(was_greater);
}

{
    // Same, but shows that brackets are not important
    was_greater := false;
    if (get_int() > 1)
        was_greater = true;

    use_bool(was_greater);
}

{
    // Fails, because was_greater is only present in the if scope, and is used
    // outside.
    if (get_int() > 1)
        was_greater := true; // _Declares_ the variable

    use_bool(was_greater);
}
