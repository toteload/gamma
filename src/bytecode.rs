struct VirtualMachine {
    stack: Vec<AnyValue>,
    call_stack: Vec<(usize, u32)>,

    base: Vec<usize>,

    pc: usize,

    globals: Vec<AnyValue>,
}

impl VirtualMachine {
    fn execute(&mut self, code: &[OpCode]) {
        self.pc = 0;

        match &code[self.pc] {
            Add => { self.stack.push(add(self.stack.pop(), self.stack.pop())); },
            LE => { self.stack.push(le(self.stack.pop(), self.stack.pop())); },

            PushConstant(val) => {
                self.stack.push(val);
            },

            Push(offset) => {
                self.stack.push(self.stack[self.base.last().unwrap() + offset]);
            }

            Pop => self.stack.pop(),

            Jump(addr) => {
                self.pc = addr;
            },

            Call{addr, arg_count} => {
                self.call_stack.push((self.pc + 1, arg_count));
                self.pc = addr;
                self.base.push(self.stack.len());
            }

            Return => {
                let (self.pc, arg_count) = self.call_stack.pop().unwrap();
                self.base.pop();
                self.stack.truncate(self.stack.len() - arg_count);
            }

            _ => todo!(),
        }
    }
}

// fn fac(n: int) int {
//     if n <= 0 {
//         1
//     } else {
//         n * fac(n - 1)
//     }
// }
//
// fn main() int {
//     fac(10)
// }

//

let code = vec![
    PushConstant(10),
    Call{addr: 3, args: 1},
    Exit,

    // fac(n: int) int
    Push(-1),        // push n
    PushConstant(0), // push 0
    LE,              // n <= 0

    JumpTrue(3),
    PushConstant(1), // push 1
    Return,          // return 1

    Push(-1),        // push n
    Push(-1),        // push n
    PushConstant(1), // push 1
    Sub,             // 2x pop => push n - 1
    Call{addr: 3, args: 1},         // call fac with (n - 1)
    Mul,             // n * fac(n - 1)
    Return,          // return n * fac(n - 1)
];

type LabelId = usize;

enum Var {
    Global(usize),
    Relative(isize),
    Constant(AnyValue),
    Function(LabelId),
}

enum OpCode {
    LE,

    Add,
    Sub,
    Mul,

    Label(LabelId),

    PushConstant(AnyValue),
    PushGlobal(usize),
    Push(isize),
    Pop,

    Jump(usize),
    JumpRelIfTrue(isize),

    Move(isize),

    Call{addr: usize, args: u32},
    Return,
}

struct BytecodeBuilder {
    code: Vec<OpCode>,

    labels: HashMap<String, usize>,
}

impl BytecodeBuilder {
    fn build_expr(&mut self, e: &Expr) {
        match &e.kind {
            BinOp(op, left, right) => {
                build_expr(left);
                build_expr(right);
                self.code.push(match op {
                    Add => OpCode::Add,
                    Sub => OpCode::Sub,
                    Mul => OpCode::Mul,
                    _ => todo!(),
                });
            },

            Call{ name, args } => {
            },

            If { cond, then, otherwise } => {
                build_expr(cond);
                todo!();
            },

            Identifer(id) => {
                let v = env.get(&id);
                match v {
                    Global(i) => self.code.push(OpCode::PushGlobal(i)),
                    Relative(i) => self.code.push(OpCode::Push(i)),
                    Constant(x) => self.code.push(OpCode::PushConstant(x)),
                }
            },

            LiteralBool(b) => self.code.push(OpCode::PushConstantBool(b)),
            LiteralInt(i) => self.code.push(OpCode::PushConstantInt(i)),
        }
    }

    fn build_function(&mut self, f: &Function) -> usize {
        let function_label = self.gen_label_id();
        self.code.push(OpCode::Label(function_label));
        self.code.push(OpCode::Return);
        todo!();
    }

    fn build_from_ast(&mut self, ast: &Toplevel) {
        for f in &ast.functions {
        }
    }
}
