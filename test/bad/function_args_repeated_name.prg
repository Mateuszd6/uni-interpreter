// FAILS: function_args_repeated_name.prg:7:1: Function argument named `x' is repeated more than once.

// Two args named the same are not allowed and we deliberately catch
// this error on function declaration, because we don't want this to
// error-out at when function is called.

fstArg :: (x : string, x : string) -> string {
    return x;
}

assert: false; // Not reached.
