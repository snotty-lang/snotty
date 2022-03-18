[Back](00basics.md) | [Table of Contents](tableofcontents.md) | [Next](02controlflow.md)
---                  | ---                                     | ---

# Variables and Data Types

# Variables

Variables are defined using the assignment keyword `let`, like so:
```
let my_var = 7
```
The type of the variables are infered by the compiler, so you don't have to specify the type. In this case, the type of `my_var` is `int`.

## Static Variables
Variables can also be static, these will be accessable in the scope the are defined in for the whole duration of the program. They act like the static variables in C.
The keyword to define a static variable is `static`. For example:
```
static my_static_var = 7
```
The type of static variables are also infered by the compiler. Value assigned to a static variable during its initialization must be known at compile time.


Note: All variables are mutable, and can be mutated

# Data Types
There are several data types available in ezlang. More will be added in the future
The types available at present are the following:
* `int`
* `char`
* `bool`
* None
* References
* Pointers
* Arrays
* Strings

## int
`int` is an 8-bit Integer. It ranges from 255 to -256, where -1 = 255. It is wraped on overflow and underflow. When printed, positive numbers will be printed, for example:
```
ezout -4
```
will print 251.

## char
`char` is another 8 bit value. It can be used to represent a single character.
```
let a = 'a'
```

## bool
`bool` is a boolean value. It can either be `true` or `false`.
```
let a = true
```

## None
The None types takes up 0 bytes of memory. It is denoted using `;`

## References
References are aliases to the variable. A `&x` will be the same as `x`, they would point to the same memory location and have the same value.

```
let a = 45
let b = &a
ezout a, *b;
*b = 4
ezout a, *b
```

Note: All references are mutable

## Pointers
Pointers are the same as pointers in other languages. They are 16bit long. They are denoted with a `*`. They can be created using the `point` keyword.

```
let a = 6
let b = *point b
```

## Arrays
Arrays are continous blocks of memory. There is no array type, arrays are used using pointers

```
let array = [0, 1, 2, 5]
ezout array[1]
```

## Strings
Strings are array for chars. They are created using double quotes, and end with a null character.

```
let string = "Hello World!"
```

# Type Conversion
The `as` keyword can be used to convert types.
```
let a = ezin as int
let b = 32 as char
let c = 0 as bool
```
