// FAILS: return_in_void.prg:4:5: Void function cannot return a value.

foobar :: ()  {
    return 4; // Fails void functions can't return a value
}

foobar(); // If fails when we call the function.
