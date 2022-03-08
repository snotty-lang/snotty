[Back](01variables.md) | [Table of Contents](tableofcontents.md) | [Next](02controlflow.md.md)
---                  | ---                                     | ---

# Conrol Flow

## If Statements
The keyword `if` and `else` are used for conditional statements.
```
if 4 > ezin as int - 43 {
    ezout 's', 'm', 'o', 'l', '\n'
} else {
    ezout 'b', 'i', 'g', '\n'
}
```

Ternary operator is also available.
```
let a = 4 == 4 ? 5: 4
```

## While Loops
While loops can be declared using the `while` keyword
```
while ezin != '\n' {
    ezout 'e', 'n', 't', 'e', 'r', '?', '\n'
}
```

## For Loops
ezlang has C like for loops
```
for (let i = 0 : i < 10 : i++) {
    ezout 'i', ':', ' ', i, '\n'
}
```

Note: There is no break or continue
