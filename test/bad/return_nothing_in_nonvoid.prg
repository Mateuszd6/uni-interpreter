// FAILS: return_nothing_in_nonvoid.prg:7:9: Return without a value when value was expected.

foobar :: (x : int) -> int {
    if ((x % 2) == 1)
        return 1;
    else
        return; // Return without a value is not allowed here.
}

foo := foobar(3); // Ok. we didn't hit the else case.
bar := foobar(4); // Fails because no value is returned.
