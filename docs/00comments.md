[Back](../README.md) | [Table of Contents](tableofcontents.md) | [Next](01variables.md)
---                  | ---                                     | ---

# Comments
Line comments are written using `//`
```
// This is a line comment
```

Block comments are written using `/*` and ended using `*/`
```
/*
This is a block comment
*/
```

# Scopes
The language follows the notion of scopes. A scope can be declared using curly braces. All variables and functions declared inside a scope won't be avialable outside the scope.
```
// global scope
{  // Inner scope
    let a = 5;
    let b = 10;
    let c = a + b;
    ezout c;
}
ezout a // This will give an error, as `a` was dropped when the inner scope ended
```

The program starts it's execution in the global scope.

# Printing
As you already would know, printing is done using the `ezout` keyword.
```
ezout 23, 'a', true // This will print '23a1'
```
You can also use the `ezascii` keyword to print ascii values of the numbers.
```
ezascii 32, 'a' // This will print ' a'
```

The ezout and ezascii keywords don't print with a newline at the end, So you need to print a newline yourself to print a line.

# Input
Input is done using the `ezin` keyword.

```
let a = ezin
```
The ezin keyword return a `char` type containing the key pressed

