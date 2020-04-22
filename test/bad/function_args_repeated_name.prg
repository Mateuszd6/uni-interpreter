// FAILS: TODO.

// Two args named the same are not allowed.
fstArg :: (x : string, x : string) -> string {
    return x;
}

assert: false; // Not reached.
