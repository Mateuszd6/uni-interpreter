square :: (x : int) -> int {
    return x * x;
}

assert: square(4) == 16;
assert: square(5) == 25;
assert: square(6) == 36;
assert: square(7) == 49;

cat3 :: (x : string, y : string, z : string) -> string {
    return x @ y @ z;
}

assert: cat3("foo", "bar", "baz") == "foobarbaz";
