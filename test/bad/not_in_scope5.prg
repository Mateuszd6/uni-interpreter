// FAILS: not_in_scope5.prg:10:10: Variable `x' not in scope.

{
    localFun :: () {
        x := 3;
        assert: x == 3;
    }

    localFun();
    y := x; // Local functions do not polute the scope with their locally
            // defined variables.
}
