// FAILS: bind_func_not_binded2.prg:24:9: Can't call function `sneaky_func' because it refers to the wider scope than the current binded block does.

// A global pure funciton. It is safe to call anywhere and we allow for it
// anywhere.
square :: (x : int)! -> int {
    return x * x;
}

// This works:
{

}

// This fails.
{
    out : int = 0;
    sneaky_func :: () {
        out = 5;
    }

    // foo is pure
    foo :: (a : int, b : int)! {
        _ = square(a) + square(b); // Perfectly fine - pure function.
        sneaky_func(); // We don't know if sneaky_func would cause some side
                       // effects, so we don't allow to call such function.
                       // In that case it does, but it is irrelevant.
    }

    foo(1, 2);
}
