// FAILS: tests.txt:6:5: Variable `bar' not in scope.

foo : int = 3;

{
    bar = foo; // bar is unknown ident, should be 'bar :='
}
