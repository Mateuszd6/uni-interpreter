// FAILS: nested_tuples2.prg:10:21: Tuple is not allowed here.

// Tuples can't be nested. It is always an error.
// This time it's a type error, because we can't recognize it when parsing.

getv3 :: () -> [int, int] {
    return [1, 2];
}

[a, b, c] := [1, 2, getv3()];
