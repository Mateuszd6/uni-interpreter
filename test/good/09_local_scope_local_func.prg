// Local variable being changed in the local function.

{
    localvar := 0;
    incLocal :: (x : int) {
        // Affect variable from the parent scope.
        localvar = localvar + x;
    }

    incLocal(4);
    incLocal(3);
    assert: localvar == 7;
}
