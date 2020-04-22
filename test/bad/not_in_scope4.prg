// FAILS: not_in_scope4.prg:7:10: Variable `i' not in scope.

{
    for (i : 0..10)
        assert: i <= 10;

    x := i; // Fails, for loop iterator is not visible outside the scope.
}
