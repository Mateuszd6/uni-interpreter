// FAILS: scope_err_struct1.prg:30:16: Type error: expected `foo', got `*unknown* (typeId = 5)'.
// Structs defined for local scope and their rules.
// TODO: Fix unknown.

foo :: struct {
    x : int;
    s : string;
}

getFoo :: () -> foo {
    retval : foo = new foo { };
    retval.x = 1;
    retval.s = "test";

    return retval;
}

{
    // The language does not feature 'compatible types' and this is a distinct
    // type from foo at 3 (like C).
    foo :: struct {
        x : int;
        s : string;
    }

    // This works, the type is deduced to foo at line 3.
    t1 := getFoo();

    // This fails as intended, foo is not the return type of getFoo in this scope.
    t2 : foo = getFoo();
}
