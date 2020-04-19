// FAILS: tests.txt:2:10: Type error: expected `int', got `string'.
for (i : "q"..3)
{
    die("not reached");
}
