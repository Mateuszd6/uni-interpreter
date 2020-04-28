// FAILS: static_typecheck2.prg:4:5: Variable `i' not in scope.

assert: false; // Not reached.
if (i <= 10) { // Static check: is is not declared.

}
