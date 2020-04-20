// FAILS: type_err4.prg:5:14: Type error: expected `string', got `int'.

// When declaring type explicitly, type must match.
q := 2; // This works.
w : string = 2; // This fails with type error.
