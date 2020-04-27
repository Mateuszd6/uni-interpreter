// FAILS: type_err5.prg:8:1: Void variable is not allowed here.

// 'Void' variables are not allowed on the RHS of the assignments and declarations.
funcThatDoesNotReturnValue :: () {
    x := 3;
}

val := funcThatDoesNotReturnValue(); // Void can't be a RHS of asignment and declaration.
