// Tuple declaration, asignment and returning from functions.

{
    // Tuples can be used to create new variables:
    [x, y] := [1, 2]; // x and y and declared here.
    assert: x == 1;
    assert: y == 2;
}

{
    // Or to assign to already existing onces (like C++'s std::tie):
    x : int;
    y : int;
    [x, y] = [1, 2];
    assert: x == 1;
    assert: y == 2;
}

returnATuple :: (x : int) -> [int, int] {
    return [x, x * x];
}

// Tuples from fucntion.
[x, y] := returnATuple(5);
assert: x == 5;
assert: y == 25;

// Same as above, but assign the existing values instead of create new.
[x, y] = [1, 10 + 10 * 10];

// Call the function that returns a tuple, but ignore a value.
_ = returnATuple(5);

// Equivalent to this:
[_, _] = returnATuple(5);

// Ignore first
[_, t] := returnATuple(5);
assert: t == 25;
