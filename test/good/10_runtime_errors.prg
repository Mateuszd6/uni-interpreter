// FAILS: 10_runtime_errors.prg:17:20: Divide by zero.
// TODO: Decide if we support uninitialized variables and if so add here an example.

getNumber :: () -> int {
    return 4;
}

getOtherNumber :: () -> int {
    return 0;
}

x := getOtherNumber() / getNumber();
assert: x == 0;

// Fails - division by 0. We don't have to evaluate the `y' (like print(y)) to
// get the error. Seams crazy I know, but that's what was happening at first.
y := getNumber() / getOtherNumber();

while (true) { } // Not reached.
