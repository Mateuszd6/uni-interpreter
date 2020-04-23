// FAILS: bind_not_binded2.prg:9:5: Variable `out' is used, but not binded.

x := 3;
y := 4;
out := 0;

!(x, y) {
    z := 4;
    !(z, out) { // Cannot bind 'out' since it was not binded in the upper scope.
        // out = z; // We don't even need to use out to get an error.
    }
}
