// FAILS: scope_repeated_decl.prg:9:9: Variable is already declared in the current scope.

foo : int = 1; // Global variable.

{
    foo : string = "test"; // This is fine, it hides foo at 3.
    {
        foo := 10; // Another hide.
        foo := 20; // Fails. Redelcare a variable in the same scope.
    }
}
