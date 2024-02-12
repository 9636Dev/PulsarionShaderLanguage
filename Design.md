# Design For PulsarionShaderLanguage

## Lexer

- [x] Read input file and produce tokens
- [ ] Integrate incremental lexing

## Parser

- [x] Parse Scopes
- [x] Parse Expressions
    - [x] Numerical Expressions
    - [x] Ternary Expressions
    - [x] Logical Expressions
    - [x] Comparisons
- [ ] Parse Keywords
    - [x] Control flow keywords
        - [x] Return
        - [ ] For
        - [ ] While
        - [ ] Break
- [ ] Check Types
    - [ ] Traverse the tree using in-order traversal
    - [ ] Stop traversal of the current subtree when it encounters an expression or statement that contains types
    - [ ] Keep track of Types of variables and functions
        - [ ] Keep track of current scope and variable / function scope
    - [ ] Evaluate to make sure assignments and operations on a variable are valid
    - [ ] Generate a helper struct with type information for optimization and code generation

## Optimizer
Not currently implemented, as it is not required

## Code Generator
- [ ] A simple C++ code generator that uses PulsarionMath for math operations


## Future Steps
- [ ] A file importing system
- [ ] Custom library written in PSHL

## Testing
- [x] Lexer Testing
- [ ] Parser Testing
    - [ ] AST Generation testing
    - [ ] Type checking testing
- [ ] Optimizer Testing
- [ ] Code Generation Testing
