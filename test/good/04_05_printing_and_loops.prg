isPrime :: (n : int) -> bool {
    if (n <= 1)
        return false;

    i := 2;
    while (i * i <= n) {
        if (n % i == 0)
            return false;
        else
            i = i + 1;
    }

    return true;
}

assert: isPrime(1) == false;
assert: isPrime(2) == true;
assert: isPrime(3) == true;
assert: isPrime(4) == false;
assert: isPrime(5) == true;
assert: isPrime(6) == false;
assert: isPrime(7) == true;

isStr :: (b : bool) -> string {
    if (b) return "is";
    else return "is not";
}

print("1241", isStr(isPrime(1241)), "a prime number"); // 1241 = 17 * 73
