// FAILS: TODO.

foo : int = 1; // Global variable.

{
    foo : string = "test"; // This is fine, it hides foo at 3.
    {
	foo := 10; // Another hide.
	foo := 20; // Fails. Redelcare a variable in the same scope.
    }
}
