//! A language, which doesn't have much. But, It can be compiled to brain fuck.


fn main() {
    ezlang::run("test.ez");
}

/*
2 + 3 - 5 * 6 / 7 % 8
print(1, 2 ,x)
sum(3, 4, 5)

Unit, Byte, Function
eg:

let thirteen = 13;
let A = thirteen + thirteen - 6;
let B = A + A;
print(sum(A, B, 2), sum(A, A, A));

to

+++++++++++++><[>+>+<<-]>>[<<+>>-]<<[>>+>+<<<-]
>>>[<<<+>>>-]<[->+<]<[->>+<<]>>><<<[-]>>[<<+>>-
]><[-]<[-]++++++><<[->>+<<]>[->-<]>><<<[-]>>[<<
+>>-]><[-]<[-]<[>+>+<<-]>>[<<+>>-]<<[>>+>+<<<-]
>>>[<<<+>>>-]<[->+<]<[->>+<<]>>><<<[-]>>[<<+>>-
]><[-]<[-]<<[>>+>+<<<-]>>>[<<<+>>>-]<<<[>>>+>+<
<<<-]>>>>[<<<<+>>>>-]<<<<[>>>>+>+<<<<<-]>>>>>[<
<<<<+>>>>>-]<[->+<]><<[->>+<<]>><<<[->>>+<<<]>>
>><<<<[-]>>>[<<<+>>>-]><[-]<[-]<[-]++><<<[>>>+>
+<<<<-]>>>>[<<<<+>>>>-]<<<<<[>>>>>+>+<<<<<<-]>>
>>>>[<<<<<<+>>>>>>-]<[->+<]><<[->>+<<]>><<<[->>
>+<<<]>>>><<<<[-]>>>[<<<+>>>-]><[-]<[-]<[-]>+++
+[<++++++++>-]<<.>.<<.>>>+++[<------->-]<-.[-]<
<[-]>>[<<+>>-]<[-]
*/
