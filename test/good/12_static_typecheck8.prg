// FAILS: 12_static_typecheck8.prg:5:5: Type error: expected `int', got `string'.

assert: false; // Not reached.
foobar :: () -> int {
    return "hehehe";
}

// q := foobar(); // No need to call foobar to get an error if static typecheck is on.
