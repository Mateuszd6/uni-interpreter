// FAILS: bind_not_binded1.prg:4:1: Variable `x' not in scope.

// Example shows how binds can be nested:
!(x, y) {
    z := 4;
    !(z) {
        z = 5;
        print(x); // x is not binded here.
    }
}
