// FAILS: static_typecheck5.prg:8:5: Type error: expected `bool', got `int'.

assert: false; // Not reached.
foo :: (x : int, y : int) -> int {
    return x * y;
}

if (foo(1, 2)) { // Function return type is checked statically.

}
