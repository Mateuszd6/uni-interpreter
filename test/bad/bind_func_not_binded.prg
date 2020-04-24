// FAILS: bind_func_not_binded.prg:7:5: Variable `out' is used, but not binded.

out : int = 4;

// foo refers to out from the outer scope, but out is not bindned (foo is pure)
foo :: (a : int, b : int)! {
    out = a + b;
}

// But we have to call this to get an error. This feature was proposed for
// compiled languages and getting this error in compile time would be much more
// useful. // TODO: Static check - this may be irrelevant?
foo(1, 2);
