// FAILS: TODO

out : int = 4;

// This function can modify only 'out' variable.
foo :: (a : int, b : int)! -> int {
    {}
    out = a + b;
    return out * out;
}

foo(1, 2);
print(out, "\n");
