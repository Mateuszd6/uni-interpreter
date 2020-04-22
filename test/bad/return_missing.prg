// FAILS: return_missing.prg:17:8: Function that returns a value didn't return.

voidFunc :: (x : int) {
    _ = x;
    // void functions can skip return.
}

foobar :: (x : int) -> int {
    // Functions which return a value must always have a return statement in the
    // execution path. This fails.
}

// TODO: Asigning void should never work, because we can then declare? a
//       variable of type void.
_ = voidFunc(1);

num := foobar(1); // Fails, no return stmt.
