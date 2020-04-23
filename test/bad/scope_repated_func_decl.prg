// FAILS: scope_repated_func_decl.prg:18:9: Function is already declared in the current scope.

foo :: (x : int) -> string { // Global Function.
    return "test";
}

{
    foo :: (x : string)  { // This is fine, it hides foo at 3.
        _ = x;
        return;
    }

    {
        foo :: (x : int) -> string { // Another hide.
            return "test";
        }

        foo :: (x : int) -> string { // Fails. Redelcare a variable in the same scope.
            return "test";
        }
    }
}
