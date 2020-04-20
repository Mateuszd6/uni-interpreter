end := 4;
sum := 0;
for (i : 1 .. end) { // Loop boundary does not have to be a constant.
    sum = sum + i;
    printString ("Adding ");
    printInt (i);
    printString (""); // TODO: Fix printing.
}
assert (sum == 10);

// Now calculate another sum, using different loop.
x := 0;
while (sum > 0) {
    x = x + sum;
    sum = sum - 1;
}
assert (x == 55);

isPrime :: (n : int) -> bool {
    if (n <= 1)
	return false;

    i := 0;
    while (i * i < n) {
	if ()

        i = i + 1;
    }
}
