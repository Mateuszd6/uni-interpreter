// FAILS: return_incorrect_type.prg:4:5: Type error: expected `int', got `string'.

foobar :: (x : int) -> int {
    return "this should never work";
}

deduceMe := foobar(0);
