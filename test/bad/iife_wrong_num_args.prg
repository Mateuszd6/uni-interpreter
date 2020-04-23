// FAILS: iife_wrong_num_args.prg:4:6: Invalid number of parameters. Expected 2, but got 1.

// Should provide two numbers instead of one:
x := (x : int, y : int) -> int { return x * y; }(1);
