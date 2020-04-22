// FAILS: nested_tuples1.prg: Parsing error: syntax error at line 4, column 12 before `['.

// Tuples can't be nested. It is always an error. This does not even parse.
_ = [1, 2, [3, 4]];
