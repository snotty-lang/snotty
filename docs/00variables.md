[Back](../README.md) | [Table of Contents](tableofcontents.md) | [Next](01integers.md)
---                  | ---                                     | ---

# Variables

Variables are defined using the assignment keyword `let`, like so:
```
let my_var = 7
```
The type of the variables are infered by the compiler, so you don't have to specify the type. In this case, the type of `my_var` is `int`.

# Static Variables
Variables can also be static, these will be accessable in the scope the are defined in for the whole duration of the program. They act like the static variables in C.
The keyword to define a static variable is `static`. For example:
```
static my_static_var = 7
```
The type of static variables are also infered by the compiler. Value assigned to a static variable during its initialization must be known at compile time.


Note: All variables are mutable, and can be mutated