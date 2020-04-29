// FAILS: new_repeated_member.prg:8:11: Struct member named `x' is repeated more than once.

foo :: struct {
    x : int;
    y : int;
}

foobar := new foo {
    x = 2,
    x = 3, // Repeated variable.
    y = 4
};