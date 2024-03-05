import os
import os.path as path
from pathlib import Path

VALID_SAMPLES_PATH = 'valid_samples'

files = [f for f in os.listdir(VALID_SAMPLES_PATH) if path.isfile(path.join(VALID_SAMPLES_PATH, f))]

FILE_HEADER = """use gamma::compiler::{Context, Options, Output};
use gamma::ink_codegen::MachineTarget;
use insta::assert_snapshot;
use std::fs;

"""

FUNCTION = """#[test]
fn {name}() {{
    let contents = fs::read_to_string("tests/valid_samples/{sample_path}").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options {{ target: MachineTarget::Windows, enable_optimizations: {optimize}, emit_llvm_ir: true, emit_asm: false, emit_object: false, }});

    let Ok(Output {{ llvm_ir: Some(output), .. }}) = result else {{
        let Err(errors) = result else {{ unreachable!() }};

        for error in errors.iter() {{
            error.print(&contents, &context.spans, &context.symbols, &context.type_tokens);
        }}

        panic!("Compilation of sample \\"{name}\\" resulted in {{}} error(s)", errors.len()); 
    }};

    assert_snapshot!("{name}", output);
}}
"""

code = (FILE_HEADER + 
        "\n".join([FUNCTION.format(sample_path=f, 
                                   name=Path(f).stem, 
                                   optimize='false',) for f in files]))

print(code.replace('\r', ''))
