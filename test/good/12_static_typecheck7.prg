// FAILS: 12_static_typecheck7.prg:9:2: Type error: expected `string', got `int'.

assert: false; // Not reached.

// Check statically var type when bulk-asigning variables.
x := 0;
y := 1;

[x, y] = ["mm", 12];
