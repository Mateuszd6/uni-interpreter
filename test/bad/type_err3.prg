// FAILS: type_err3.prg:11:5: Can't compare types `int' and `string'. Only builtin types with matching type can be compared.

int_var : int = 3;
int_var2 : int = 4;

str_var : string = "foo";
str_var2 : string = "bar";

_ = int_var == int_var2; // Compare ints.
_ = str_var == str_var2; // Compare strings.
_ = int_var == str_var; // Fails with type error, can't compare.
die ("Not reached.");
