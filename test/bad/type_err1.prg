// FAILS: type_err1.prg:5:5: Type error: expected `bool', got `string'.

foo := 1;

if ("boo") // here
    foo := 3;
