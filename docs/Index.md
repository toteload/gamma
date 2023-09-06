Goals:
- Gaining experience writing compilers.
- Having finished a fully functioning compiler for a C-like programming language.
- Use LLVM to generate code.
- Practice using Git more effectively.
- Build many tests while developing to test all the features.
	- Use the `insta` crate for snapshot testing. I hope that this will make writing tests less tedious, which should lower the barrier of adding tests.
- Use semantic versioning during development. Hopefully this gives a nice feeling of progression 😀

Maybe goals:
- Try out fuzzing for testing.

Non-goals:
- Fancy macro system.
- Type inference.
	- All variables must be type annotated.
- Advanced type system with generics.
- Memory safety features.
- Varargs.

### Language features and specification
- Has a type system
	- Primitive types
		- void
		- bool
		- unsigned and signed integers of size 8, 16, 32, 64.
		- f32, f64
	- Other types
		- string
		- pointer
		- array
		- function pointer
		- struct
		- union
- Top level definitions may be in any order.
- Variables are always mutable.
- Has user defined functions.
- Logical operators for booleans.
- Comparison and arithmetic operators for integers and floats.
- Control flow constructs
	- if / else
	- loop (an infinite loop)
		- break
		- continue
	- return
- Control flow analysis
	- Ensure that a function returns a value.
	- Ensure that break and continue are only used within a loop.
 - Can call external C code.
 - Only explicit type casting.



```
fn fib(n: int) -> int {
	if n == 0 || n == 1 {
		return 1;
	} else {
		return fib(n - 1) + fib(n - 2)
	}
}

fn fac(n: int) -> int {
	let res: int = 1;
  let n: int = n;
	loop {
    if n == 0 {
			return res;
    }

		set res = res * n;
		set n = n - 1;
	}
}

fn fac(n int) int
	let res int = 1
	let n   int = n
	
	loop
		if (= n 0)
			return res
		end

		set res = (* res n)
		set n = (- n 1)
	end
end

fn fib(n: int): int
	if (or (= n 0) (= n 1))
		return 1
	else
		 return (+ (fib (- n 1)) (fib (- n 2)))
	end
end

fn main(): int
	let x: bool = false let n: int = (cast []int x)
end
```