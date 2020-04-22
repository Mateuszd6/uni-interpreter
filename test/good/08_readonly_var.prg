// FAILS: 08_readonly_var.prg:12:5: Variable is read only.

{
    x := 3; // Regular var
    y := x + 3; // can be used in an expression
    x = y; // or reassigned
}

{
    x! := 3; // Readonly var
    y := x + 3; // can be used in an expression
    x = y; // but fails when trying to reassign it.
}
