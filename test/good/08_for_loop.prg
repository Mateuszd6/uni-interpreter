// For loops. Iterator variable is read-only. Check corresponding 'bad' test for
// demonstartion.

max := 10;
sum := 0;
for (i : 0 .. max) { // i goes from 0 to max (inclusive)
    sum = sum + i;
}

assert: sum == 55;

m := 1;
r := 0;
for (i : 8 .. 2) { // i goes down from 8 to 2 (inclusive both ways)
    r = r + m * i;
    m = m * 10;
}

assert: r == 2345678;


// Fors can be nested.
total := 0;
for (i : 1 .. 10)
    for (j : 1 .. 10)
        total = total + 1;

assert: total == 100;
