[Back](02controlflow.md) | [Table of Contents](tableofcontents.md) | [Next](04functions.md)
---                  | ---                                     | ---

# Preprocessor

ezlang has a preprocessor. It is used to replace tokens, define and include other files.
All preprocessor directives are prefixed with `!`.

## `!use`
Include a file
`!use <filename> | "<filepath>"`


```
!use stdlib  // Includes stdlib.ez
!use "../myfile.ez" // Includes myfile.ez which is in the above directory
```

When written without quotes, the file searched is filename+`.ez`

## `!replace`
Replace a token with other tokens
`!replace <token> <replace> | "<replace>"`


```
!replace TRUE true
let a = TRUE
```
```
!replace FIVE "3 + 2"
let a = FIVE
```
The replace portion can also be in quotes, in that case the string is lexed are the find portion is replaced by that list of tokens.

## `!declare`, `!ifdeclared`, `else` and `endif`
Declared a flag, or check if a flag is declared
```
!declare FLAG

!ifdeclared FLAG
ezout 23
!else
ezout 34
!endif
```

Useful for not including files twice
