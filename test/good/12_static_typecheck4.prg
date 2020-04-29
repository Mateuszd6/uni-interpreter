// FAILS: 12_static_typecheck4.prg:5:9: Type error: expected `bool', got `int'.

// assert: false; // Not reached.
foo :: (x : int, y : int) -> int {
    if (x) { // Static check failed: expected bool.
    }

    return x * y;
}
