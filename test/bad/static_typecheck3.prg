// FAILS: static_typecheck3.prg:14:5: Type error: expected `bool', got `int'.
assert: false; // Not reached.

bar :: struct {
    y : int;
}

foo :: struct {
    x : bar;
}

v : foo;
v.x.y = 10; // Not necesarry, but for clarity.
if (v.x.y) { // Static check: v.x is of type int, not bool

}
