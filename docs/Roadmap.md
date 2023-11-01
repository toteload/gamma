- [ ] Write tests for existing features.
- [x] Write a roadmap with semantic versioning.
- [ ] Write a grammar description.

### Version 0.1
Compiler is able to compile a basic program. Vertically speaking, most of the components are present.

- Tokenizer
	- [x] Make one
	- [x] Write tests
- [x] Move tokenizer snapshot test to `tokenizer.rs`
- Parser
	- [x] Make one
	- [ ] Write tests
- Semantics checker
	- [x] Program must contain a main function.
	- [x] Organize code
	- [x] Give the undefined names pass a different name. It does more than just find undefined names.
- [ ] Flatten AST
	- Leave this for another time. It will change the way you interact with the AST in many different places. There could be an elegant way of handling it, but eh some other time. It also reminds of flat structure for S-expressions, where you have the challenge of storing lists (like arguments) in a flat structure. (Terrible explanation, I know, but I am too tired to explain it any better.)
- Code generator
	- [x] Use LLVM to generate IR

### Version 0.2
- [x] Add bool
- [x] Port existing code generator to use inkwell
- Code generator
	- [x] Logical operators
	- [ ] Comparison operators
	- [x] arithmetic operators (maybe already done)
	- [x] Bitwise ops
	- [x] Casting behavior
		- `bool -> int` : `true -> 1` and `false -> 0`
		- `int -> bool` : `0 -> false` and other values evaluate to `true`
	- [x] `if`/`else`
- [x] Add logical operators that only work for bools
	- and or not
	- `&&` `||` `!`
	- [x] Add tokens
	- [x] Add to parser and AST.
- [x] Add all signed and unsigned integer types. 
	- Maybe don't do this and only provide `int` at this stage?
	- I am only allowing `int` at this point, which is a 64-bit signed integer.
- [x] Add comparison operators
	- `==` `!=` `>=` `<=` `>` `<`
	- [x] Add tokens
	- [x] Add to parser and AST.
 - [x] Add bitwise operators
	 - not and or xor
	 - [x] Add tokens
	 - [x] Add to parser and AST.
 - [ ] Add arithmetic operators
	 - `+` `-` `*` `/`
	 - [x] Add tokens
	 - [x] Add to parser.
	 - [ ] Add modulo
- Semantics checker 
	- [x] Verify that all used names are defined.
	- [ ] Disallow functions with the same name. This is away from the happy flow :) so not important now
- [x] Add `if`/`else` statement.
- [x] Type check return type.
- [ ] Add control flow analysis.
	- [ ] Ensure that all branches return a value (if the function should return a value).
- Type checker
	- [x] Condition of `if` statement must be bool
	- [x] Logical operators can only be used on bools
	- [x] == and != can be used on bools and integers
	- [x] other comparison operators can be used on integers
	- [x] Check if `cast` usage is valid, only allowed to cast between `int` and `bool`
- [x] Add type casting of int to bool
	- [x] What kind of syntax?
		- `cast(bool, 456)` or `456 as bool`. I choose `cast(bool, 456)`; the syntax is easier. `(cast bool 456)`

### Version 0.3
- [x] Add void.
- [x] Change all expressions to be of the form `(op ...)`. This way you don't have to do any operator precedence stuff. Also, there is a clear distinction between statements and expressions.
- [x] Add `trap` statement to abort the program
	- [x] Add to codegen
	- [x] Remove the `trap` statement
- [ ] A function must have a `return` statement, even functions that return `void`.
- [ ] Add `exit` statement to exit the program with an exit code.
	- [ ] This means the program has a small runtime, similar to C. On Windows `ExitProcess` needs to be called and on Linux `exit`.
	- This can be used to create programs that check for conditions and exit with a specific code to signal that something is wrong. This can be used in a test suite, where the programs are compiled and ran, and the return code is verified.
- [ ] `main` function must be of the correct type `fn(): void` or `fn(): int` ?? Not sure which one to choose.
- [x] Add `let` statement.
	- Variable initialization is optional. This can also be skipped entirely, to keep the language simpler.
	- This could be added, and implemented with a small AST transform where the `let` with initializer get transformed into a `let` without initializer and a `set`.
 - [x] Add assignment operator
	 - I am thinking something like `set x = ...;`. This makes parsing super easy, because it starts with a keyword.
	 - This set syntax may be OK for a Lisp like language, but I don't like it for this C-like language.
	 - And when I think about it now; I do like it again! This way all statements start with a unique keyword
- [ ] `void` can only be used as the return type of a function
- Testing
	- [x] Set up a way to run sample programs and compare output.
- [x] Add data layout to LLVM module
	- I don't do this.
- [ ] Make end of `SourceSpan` exclusive so that you can have empty spans.
- [x] Track down what causes the random errors. Sometimes the compiler encounters even though the input is the same `todo`.
	- [ ] Serialize the AST and all the corresponding data, like spans. This way you can check that these are all consistent across compilations.
	- The problem turned out to be in the implementation of `PartialEq` for `Type`. It was a very lame mistake... The last line should be `let y = other.tag`, but I was using `self` on both lines...
```rust
impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        use Type::*;
        let x = self.tag();
        let y = self.tag(); 
		...
```
- [ ] Change syntax
	- [x] Use `end` instead of using braces.
	- [ ] The `end` for an if-statement could be optional if there is an else afterwards
	- [x] Remove arrow from function syntax
- [x] Use the compiler error setup described in this blog post https://borretti.me/article/lessons-writing-compiler#err
- [x] Add optional initializer to `let` statement. This could be implemented with an AST modification.
- [ ] Add script to list all TODO
- [x] Add script to generate sample tests from root
- [x] Add error printing function
- [ ] Verify arity of builtin functions
### Version 0.4.1
- [ ] Start development of Tetris game in parallel to the compiler. Let the needs of the game steer the development direction of the compiler.
- [ ] Add functionality for calling externally defined C functions.
	- [ ] Syntax for declaring externally defined functions.
	- [ ] C types:
		- [x] Integers
		- [ ] structs
			- Struct indexing syntax `player.name`
		- [x] pointers
		- [ ] ...
### Version 0.4
- [ ] Add pointer type.
	- `^T`
	- [x] Add dereferencing operator.
		- `(@ p)`
	- [x] Add operator to take the address of a variable. `(& x)`
		- [ ] Taking the address of a function is illegal
	- [ ] Add pointer arithmetic.
	- [x] Add dereferencing in the destination part of `set` statement
	- [ ] Add type casting of one pointer type to another pointer type
	- [ ] Add type casting of `u64` to a pointer and vice versa
	- [ ] Add comparison operators
- [ ] Add array type.
	- `[128]T` or `[T:128]`. I prefer the first.
	- [x] Add indexing operator. `(@ p 12)`
	- [ ] Indexing is only allowed on arrays and pointers, and can only be indexed by integers.
### Version 0.5
- [ ] Add user defined functions.
- [x] Add `loop`, `break` and `continue`
	- [x] Semantic check: `break` and `continue` can only be used inside a loop
	- [ ] Add optional labels for `loop`. You can `break :label` or `continue :label`. A label has to start with a colon.
	- `loop :main { break :main }`
- [ ] Add modules
- [ ] Add `alloc` and `free` functions for dynamic memory allocations, to a module called `std`
	- [ ] The `std` module will be a special hard coded module (?)

### Version 0.6
- [ ] Add union type
	- [ ] Add union value syntax so that a union can be properly initialized.
- [ ] Add struct type
	- [ ] Add struct value syntax so that a struct can be properly initialized.
- [ ] Add field selection syntax for structs and unions

### Version 0.7
- [ ] Add global variables
- [ ] Add `f32` and `f64`
	- [ ] Add casting between integers and floats.
	- [ ] Add float constant lexing
 - [ ] Add function pointer type
	 - [ ] Dereferencing a function pointer is illegal.
	 - [ ] Taking the address of a function is now legal? What is the syntax of getting the address of a function?

### Version 0.8
- [ ] Add string type