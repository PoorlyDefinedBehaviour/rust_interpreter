Toy language for learning purposes.

Based on: </br>

- http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
- https://interpreterbook.com/
- https://craftinginterpreters.com/

```rust
bruno@bruno:~/dev/rust/interpreter_1$ cargo run
   Compiling interpreter v0.1.0 (/home/bruno/dev/rust/interpreter_1)
    Finished dev [unoptimized + debuginfo] target(s) in 0.23s
     Running `target/debug/interpreter`
> x |> f |> g
[src/main.rs:26] program.statements = [
    Expression(
        Infix(
            InfixExpression {
                left_operand: Infix(
                    InfixExpression {
                        left_operand: Identifier(
                            "x",
                        ),
                        operator: Pipe,
                        right_operand: Identifier(
                            "f",
                        ),
                    },
                ),
                operator: Pipe,
                right_operand: Identifier(
                    "g",
                ),
            },
        ),
    ),
]
```

```rust
bruno@bruno:~/dev/rust/interpreter_1$ cargo run
   Compiling interpreter v0.1.0 (/home/bruno/dev/rust/interpreter_1)
    Finished dev [unoptimized + debuginfo] target(s) in 0.28s
     Running `target/debug/interpreter`
> let factorial = fn(x) { if(x <= 1) { 1 } else { factorial(x - 1) * x } }
[src/main.rs:26] program.statements = [
    Let(
        LetStatement {
            token: Let,
            identifier: Identifier(
                "factorial",
            ),
            value: Function(
                FunctionExpression {
                    parameters: [
                        Identifier(
                            "x",
                        ),
                    ],
                    body: Block(
                        [
                            Expression(
                                If(
                                    IfExpression {
                                        condition: Infix(
                                            InfixExpression {
                                                left_operand: Identifier(
                                                    "x",
                                                ),
                                                operator: LessThanOrEqual,
                                                right_operand: Number(
                                                    1.0,
                                                ),
                                            },
                                        ),
                                        consequence: Block(
                                            [
                                                Expression(
                                                    Number(
                                                        1.0,
                                                    ),
                                                ),
                                            ],
                                        ),
                                        alternative: Some(
                                            Block(
                                                [
                                                    Expression(
                                                        Infix(
                                                            InfixExpression {
                                                                left_operand: Call(
                                                                    CallExpression {
                                                                        function: Identifier(
                                                                            "factorial",
                                                                        ),
                                                                        arguments: [
                                                                            Infix(
                                                                                InfixExpression {
                                                                                    left_operand: Identifier(
                                                                                        "x",
                                                                                    ),
                                                                                    operator: Minus,
                                                                                    right_operand: Number(
                                                                                        1.0,
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        ],
                                                                    },
                                                                ),
                                                                operator: Star,
                                                                right_operand: Identifier(
                                                                    "x",
                                                                ),
                                                            },
                                                        ),
                                                    ),
                                                ],
                                            ),
                                        ),
                                    },
                                ),
                            ),
                        ],
                    ),
                },
            ),
        },
    ),
]
```
