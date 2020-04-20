// Recursion, nested functions, and tuples.

// Assumes n >= 1.
fib :: (n: int) -> int {
    fibImpl :: (n : int) -> [int, int] { // Nested fucntion that returns a tuple
        if (n <= 1)
            return [0, 1];
        else {
            [n', n] := fibImpl(n - 1);
            return [n, n + n'];
        }
    }

    [_, r] := fibImpl(n);
    return r;
}

assert: fib(1) == 1;
assert: fib(2) == 1;
assert: fib(3) == 2;
assert: fib(4) == 3;
assert: fib(5) == 5;
assert: fib(6) == 8;
assert: fib(7) == 13;
