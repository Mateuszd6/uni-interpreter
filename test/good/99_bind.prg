x := 4;
y := 5;
out : int = 0;
not_binded : int = 3;
!(x, y, out) {
    z := 5; // Variables defined in the binded block are of cource accessible.

    {
        x = x + z;
        y = y - x;
        x = x * y;
        y = x - 5;
        out = x + y;

        {
            not_binded := 3; // Declaring a var shadowing non-bindend one is fine
            not_binded = 3; // In that case, assignment works as indented.
        }

        // not_binded = 3; // This would case an error. Refer to the corresponding
                           // 'bad' test for an example.
    }
}

not_binded = 3; // After leaving a bind block, we can use all vars again.

// 'Pure' square function. It is not allowed to modify any global variable.
// Error example in the 'bad' tests.
square :: (a : int)! -> int {
    return a * a;
}

// This function can modify only 'out' variable.
foo :: (a : int, b : int) !(out) -> int {
    out = a + b;
    return out * out;
}

assert: square(4) == 16;
assert: foo(1, 2) == 9;
assert: out == 3; // out was assigned when calling foo.


{
    // Global function that binds out.
    z := 0;
    out : int = 0;
    fine_func :: () !(out) {
        out = 5;
    }

    // foo binds z and out...
    foo :: (a : int, b : int) !(z, out) {
        fine_func(); // ... so it is safe to call a func that binds only out.
    }

    foo(1, 2);
}
