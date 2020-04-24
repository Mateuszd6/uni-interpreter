// FAILS: bind_func_not_binded3.prg:23:9: Variable `z' is used, but not binded.

// global variable:
out := 0;

// The same would be eqi
foo :: (a : int, b : int) !(out) {
    fine_func :: () {
        out = 5;
    }

    fine_func(); // It is safe to call this func. Since it does not
                 // have its own bind it can refer to the same
                 // variables as foo does.
}

foo(1, 2);

z := 0;
bar :: (a : int, b : int) !(out) {
    bad_func :: () {
        out = 5;
        z = 4; // Error - refers to variable that is not bindned Even though the
               // function itself does not have a bind param.
    }

    bad_func();
}

bar(1, 2);
