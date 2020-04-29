// FAILS: 12_static_typecheck1.prg:4:5: Type error: expected `bool', got `int'.

assert: false; // Not reached.
if (4) { // Static check: Should be bool

}
