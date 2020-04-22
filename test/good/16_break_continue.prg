// Break and continue.

while (true)
{
    break;
    while (true) { } // Not reached.
}

// Find numbers which deivide n:
n := 2 * 3 * 7 * 11;
a := 1;
while (a <= n)
{
    if ((n % a) != 0 ) {
        a = a + 1;
        continue;
    }

    // Not reached if continued.
    assert: (n % a) == 0;
    a = a + 1;
}

// Also for for for loops:
for (i : 0..10)
{
    i = i - 2; // This loop would never end...
    if ((i % 10) != 0)
	continue;

    break;     // ...If not this break statement.
    while (true) { } // Not reached.
}
