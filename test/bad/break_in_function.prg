// FAILS: break_in_function.prg:6:9: `break' is not allowed here.

i := 0;
while (i < 5) {
    foo :: () {
        break; // Here break is also not allowed.
    }

    foo();
    i = i + 1;
}
