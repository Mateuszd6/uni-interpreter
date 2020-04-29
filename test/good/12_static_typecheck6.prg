// FAILS: 12_static_typecheck6.prg:14:8: Type error: expected `int', got `string'.

// Static type check in 'new' field assignment.
assert: false; // Not reached.

foobar :: () -> string {
    return "";
}

zzz :: struct {
    x : int;
}

foo := new zzz { x = foobar() };
