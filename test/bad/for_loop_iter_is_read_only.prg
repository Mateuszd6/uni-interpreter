// FAILS: for_loop_iter_is_read_only.prg:4:5: Variable is read only.

for (i : 0 .. 10) {
    i = i + 1; // Fails here, cannot change the iter variable.
}
