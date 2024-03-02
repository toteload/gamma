# Gamma

This project is a compiler written in Rust for the Gamma programming language.
Gamma is a programming language with C-like semantics.
C is also where Gamma gets its name from; just like C is the third letter in the Latin alphabet, Gamma is the third letter in the ancient Greek alphabet.
It was one of my goals to write a compiler, but I didn't want the project to be open-ended.
You can always add language features, improve error messaging, optimize code generation etc.
This is why I set the goal of writing a Tetris clone in Gamma with SDL2.
I implement features to Gamma that I need to achieve this goal, but no more.
For example, I probably will not add an import or module feature to Gamma, which means that all code must be written in one file.
Because of this, the language can be tedious to use in some places.

Now, for the language... 
Below is an example program, that loops for 10 iterations and then returns the number of iterations. 
The language is white-space-insensitive and you don't need semi-colons (`;`). 
This is because each statement starts with a unique keyword, like `let`, `loop`, `if` or `return`, and each expression is either a single value, like `s` or `10`, or something that needs to be evaluated and is written between parentheses, like `(+ s 1)`.
This way there is no ambiguity and semicolons and braces can be left out.

```
fn main(): i32
  let s: i64 = 0

  loop
    if (eq s 10)
      break
    end

    set s = (+ s 1)
  end

  return (cast i32 s)
end
```

