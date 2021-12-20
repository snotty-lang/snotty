//! A language, which doesn't have much. But, It can be compiled to brainfuck.

// mod interpreter;
mod lexer;
mod parser;
mod utils;

fn main() {
    let tokens = lexer::Lexer::lex("let x=x + 4");
    println!("{:?}", tokens);
    let mut parser = parser::Parser::new(tokens.unwrap()).unwrap();
    let ast = parser.parse();
    println!("{:#?}", ast);
    // let result = interpreter::Interpreter::visit(ast.unwrap());
    // println!("{:?}", result.unwrap());
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
