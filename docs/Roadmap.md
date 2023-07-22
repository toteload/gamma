- [ ] Write tests for existing features.
- [x] Write a roadmap with semantic versioning.
- [ ] Write a grammar description.

### Version 0.1
Compiler is able to compile a basic program. Vertically speaking, most of the components are present.

- Tokenizer
	- [x] Make one
	- [x] Write tests
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
	- [ ] Logical operators
	- [ ] Comparison operators
	- [ ] arithmetic operators (maybe already done)
	- [ ] Bitwise ops
	- [ ] Casting behavior
		- `bool -> int` : `true -> 1` and `false -> 0`
		- `int -> bool` : `0 -> false` and other values evaluate to `true`
	- [x] `if`/`else`
- [x] Add logical operators that only work for bools
	- `&&` `||` `!`
	- [x] Add tokens
	- [x] Add to parser and AST.
- [ ] Add all signed and unsigned integer types. 
	- Maybe don't do this and only provide `int` at this stage?
	- [ ] Remove temporary `int` type.
- [x] Add comparison operators
	- `==` `!=` `>=` `<=` `>` `<`
	- [x] Add tokens
	- [x] Add to parser and AST.
 - [x] Add bitwise operators
	 - `!` `&` `|` xor
	 - [x] Add tokens
	 - [x] Add to parser and AST.
 - [ ] Add arithmetic operators
	 - `+` `-` `*` `/`
	 - [x] Add tokens
	 - [x] Add to parser.
	 - [ ] Add modulo
- Semantics checker 
	- [x] Verify that all used names are defined.
	- [ ] Disallow functions with the same name. This is more of a nice to have.
- [x] Add `if`/`else` statement.
- [ ] Add control flow analysis.
	- [ ] Ensure that all branches return a value (if the function should return a value).
- Type checker
	- [x] Condition of `if` statement must be bool
	- [ ] `main` function must be of the correct type `fn() -> int`.
	- [x] Logical operators can only be used on bools
	- [x] == and != can be used on bools and integers
	- [x] other comparison operators can be used on integers
	- [x] Check if `cast` usage is valid, only allowed to cast between `int` and `bool`
- [x] Add type casting of int to bool
	- [x] What kind of syntax?
		- `cast(bool, 456)` or `456 as bool`. I choose `cast(bool, 456)`; the syntax is easier.

### Version 0.3
- [x] Add void.
- [ ] Add user defined functions.
- [ ] Add `loop`, `break` and `continue`
	- [ ] Semantic check: `break` and `continue` can only be used inside a loop
	- [ ] Add optional labels for `loop`. You can `break :label` or `continue :label`.
- [ ] Add `let` statement.
	- Variable initialization is optional.
 - [ ] Add assignment operator
	 - I am thinking something like `set x = ...;`. This makes parsing super easy, because it starts with a keyword.
	 - This set syntax may be OK for a Lisp like language, but I don't like it for this C-like language.
	 - And when I think about it now; I do like it again!
- [ ] `void` can only be used as the return type of a function
- Testing
	- [ ] Set up a way to run generated programs and compare output.


### Version 0.4
- [ ] Add pointer type.
	- [ ] Add dereferencing operator.
	- [ ] Add operator to take the address of a variable.
		- [ ] Taking the address of a function is illegal
	- [ ] Add pointer arithmetic.
	- [ ] Add type casting of one pointer type to another pointer type
	- [ ] Add type casting of `u64` to a pointer and vice versa
	- [ ] Add comparison operators
- [ ] Add array type.
	- [ ] Add indexing operator.
	- [ ] Indexing is only allowed on arrays and pointers and can only be indexed by integers.

### Version 0.5
- [ ] Add modules
- [ ] Add `alloc` and `free` functions for dynamic memory allocations, to a module called `std`
	- [ ] The `std` module will be a special hard coded module (?)

### Version 0.6
- [ ] Add union type
	- [ ] Add union value syntax so that a union can be properly initialized.
- [ ] Add struct type
	- [ ] Add struct value syntax so that a struct can be properly initialized.
- [ ] Add field selection syntax for structs and unions
- [ ] Add `f32` and `f64`
	- [ ] Add casting between integers and floats.
	- [ ] Add float constant lexing
 - [ ] Add function pointer type
	 - [ ] Dereferencing a function pointer is illegal.
	 - [ ] Taking the address of a function is now legal? What is the syntax of getting the address of a function?

### Version 0.7
- [ ] Add global variables
- [ ] Add string type

### Version 0.8
- [ ] Add IO functions to `std` module 
	- [ ] Console reading and writing
	- [ ] File reading and writing.

### Version 0.9
- [ ] Add calling external functions (FFI).

### Version 1.0