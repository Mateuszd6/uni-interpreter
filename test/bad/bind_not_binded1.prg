// FAILS: bind_not_binded1.prg:11:15: Variable `x' is used, but not binded.

x := 0;
y := 0;

// Example shows how binds can be nested:
!(x, y) {
    z := 4;
    !(z) {
        z = 5;
        print(x); // x is not binded here.
    }
}
