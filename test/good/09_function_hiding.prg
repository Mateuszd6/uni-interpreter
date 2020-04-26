g_test := 0;

{
    test1 :: () {
        g_test = 10;
    }

    test2 :: () {
        test1();
    }

    {
        test1 :: () {
            // This function shadows test1().
            g_test = 100;
        }

        test2(); // test2 wraps test1 and correctly calls the first function.
    }
}

assert: g_test == 10;
