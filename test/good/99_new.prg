// 'new' syntax is basically the C#'s ripoff and allows us to recursively
// instantiate struct fields.
v3 :: struct {
    x : int;
    y : int;
    z : int;
}

v3line :: struct {
    point : v3;
    dir : v3;
}

p1 := new v3line {
    point = new v3 { x = 4, y = 3, z = 1 },
    dir   = new v3 { x = 0, y = 5, z = 5 }
};

assert: p1.point.x == 4;
assert: p1.point.y == 3;
assert: p1.point.z == 1;
assert: p1.dir.x == 0;
assert: p1.dir.y == 5;
assert: p1.dir.z == 5;


// Since it is an expression, it can be used to pass structs into functions
// without giving them names. Also user is not required to set up all fields and
// since language does not supports null, not initialized field will have its
// type default value (like 0 for its etc).  Note that `new v3line { point.x = 0
// }` would not parse it is not supported (like it is not supported in C#,
// although I suspect for different reasons...)
foo :: (p : v3line) { }
foo (new v3line { point = new v3 { } });
