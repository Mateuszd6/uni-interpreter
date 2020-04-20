// As a sidenote: operators '==' and '!=' would work with any builtin
// type. Other operators (like '+') are strictly typed and will cause error if
// used inproperly. See test bad/TODO for exaples.

{
    int_var : int = 3;
    int_var2 := 4; // Deduced type.
    sum := int_var + int_var2;
    mod := int_var2 ^ 3;

    assert(sum == 7);
    assert(mod == 1);
}

{
    string_var : string = "foo";
    string_var2 := "bar"; // Deduced type.
    cat := string_var @ string_var2;

    assert(cat == "foobar");
}
