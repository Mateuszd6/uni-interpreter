// FAILS: type_err2.prg:2:10: Type error: expected `int', got `string'.
for (i : "q"..3) // here ("q")
{
    die("not reached");
}
