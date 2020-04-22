// FAILS: return_outermost_scope.prg:5:1: `return' is not allowed here.

x := 2;
y := 2;
return x + y; // Return in outermost scope (or generally speaking not in a
              // function is not allowed.)
