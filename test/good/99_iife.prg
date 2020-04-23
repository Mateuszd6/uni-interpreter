// Loop from 5-9..0-4 using iife to increment i.
m := 1;
start := 5;
r := 0;
i := start;
while (true) {
    print(i, "\n");
    r = r + (i * m);
    m = m * 10;

    // Here iife is used to incremnet i only in once place. This unnamed
    // function takes no args, and returns next element to iterate. We could use
    // bind, like i = () !(i) -> ... to make sure the only var used in the iife
    // is i, but lets keep the feature tests as separate as possible.
    i = () -> int { if (i == 9) return 0; else return i + 1; }();
    if (i == start)
        break;
}

// I was: 5, 6, 7, 8, 9, 0, 1, 2, 3, 4
assert: r == 4321098765;

// An example where iife is actually usefull would be to avoid empty
// initializing an object and then asigning it in an if statement.

{
    x := 13;
    x_str : string = () !(x) -> string {
                if (x == 11)
                    return "eleven";
                else if (x == 12)
                    return "twelve";
                else if (x == 13)
                    return "thirteen";
                else // .. and so on
                    return "too much";
            }();

    assert: x_str == "thirteen";
}

// However same can be achieved using a local function.
{
    x := 13;
    conv :: (x : int) -> string {
        if (x == 11)
            return "eleven";
        else if (x == 12)
            return "twelve";
        else if (x == 13)
            return "thirteen";
        else // .. and so on
            return "too much";
    };

    x_str := conv(x);
    assert: x_str == "thirteen";
}
