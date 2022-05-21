[Back](03preprocessor.md) | [Table of Contents](tableofcontents.md)
---                  | ---

# Functions
Functions are declared using the `ez` keyword
```
ez myfunc() {
    ezout 'H', 'i', '\n'
}
```

In ezlang, the functions are like macros, they get expanded when they are called.

For Arguments, the name of the argument followed by `:` and the type of the argument.
```
ez take_args(arg1: int, arg2: bool, args: &char) { .. }
```

To mention the return type, Put an arrow after the function and the type
```
ez return_smth() -> char { .. }
```
If no return type is mentioned, the defualt type is None.
If a return type is mentioned, but nothing is returned, the default value returned will be 0.

## return
The `return` keyword in ezlang is very different to `return` in other languages. The `return` keyword doesn't terminate the function. The function still continues running.
There is a 'return spot' for every function, where its returned value is stored. The allocation of that spot is done even if you dont return anything. The value in the return spot is what gets returned to the caller. The return spot gets initialized to 0.
The `return` keyword puts the value in the return spot

```
ez returning() -> char {  // The return spot is initialized to 0 and type char
    let a = ezin
    return a  // 'a' gets put in the return spot
    ezout a, '\n'  // This still runs
}

let x = returning()  // x will be the inputted char
```
