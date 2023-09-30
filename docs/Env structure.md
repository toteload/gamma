Maybe you can call `gen_statement` recursively for each statement in a block?
This way whenever you encounter a new `let` you can introduce a new `Env` and pass it along.
Maybe this won't work because then `gen_statement` would also need to be able to retrieve the next possible statement.
Currently, `gen_statement` does not have this information.

The problem boils down to either change the structure of how the environment is accessed or change the control flow so that the environment can stay the same.
I prefer keeping the control flow the same, so that I can stick to the Visitor pattern.
This would mean making the environment part of the code generator instead of it being a parameter that gets passed in. Seems very acceptable.

#2023-09-27