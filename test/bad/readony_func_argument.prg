// FAILS: readony_func_argument.prg:6:5: Variable is read only.

foo :: (x! : int, y : int) {
    // y can be reassigned in the function body, x cannot.
    y = 4;
    x = 4; // Fails.
}

foo(1, 2);