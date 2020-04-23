// FAILS: struct_field_repeated.prg:3:1: Struct member named `x' is repeated more than once.

foobar :: struct {
    x : int;
    x : int;
}

while (true) { } // Not reached.
