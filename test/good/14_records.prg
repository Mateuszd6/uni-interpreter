// Example of structs functions that return them.

v3 :: struct {
    x : int;
    y : int;
    z : int;
}

v3dp :: (i : v3, j : v3) -> int {
    return i.x * j.x + i.y * j.y + i.z * j.z;
}

// I don't really claim that the equation is correct...
v3cp :: (i : v3, j : v3) -> int {
    return (i.y * j.z - i.z * j.y)
         - (i.x * j.z - i.z * j.x)
         + (i.x * j.y - i.y * j.x);
}

v3add :: (i : v3, j : v3) -> v3 {
    retval : v3;
    retval.x = i.x + j.x;
    retval.y = i.y + j.y;
    retval.z = i.z + j.z;

    return retval;
}


{
    t1 : v3;
    t1.x = 40;
    t1.y = 0;
    t1.z = 0;
    t2 : v3;
    t2.x = 0;
    t2.y = 12;
    t2.z = 30;
    assert: v3dp(t1, t2) == 0;

    sum : v3 = v3add(t1, t2); // Function that returns a struct.
    assert: sum.x == 40;
    assert: sum.y == 12;
    assert: sum.z == 30;
}

{
    t1 : v3;
    t1.x = 12;
    t1.y = 30;
    t1.z = 0-90;
    t2 : v3 = new v3 { };
    t2.x = (0-12);
    t2.y = (0-30);
    t2.z = 90;
    assert: v3cp(t1, t2) == 0;
}
