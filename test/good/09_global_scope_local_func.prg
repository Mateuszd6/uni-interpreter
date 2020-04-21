// Global variable being changed in the local function.

globalvar := 0;

{
    incGlobal :: (x : int) {
        // Affect variable from the parent scope.
        globalvar = globalvar + x;
    }

    incGlobal(4);
    incGlobal(3);
    assert: globalvar == 7;
}
