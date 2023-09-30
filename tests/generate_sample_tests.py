import os
import os.path as path
from pathlib import Path

VALID_SAMPLES_PATH = 'valid_samples'

files = [f for f in os.listdir(VALID_SAMPLES_PATH) if path.isfile(path.join(VALID_SAMPLES_PATH, f))]

FILE_TEMPLATE = """mod valid_samples {{
use gamma::compiler::{{Context, Options}};
use insta::assert_snapshot;
use std::fs;

{}
}}"""

FUNCTION_TEMPLATE = """#[test]
fn {stem}() {{
    let contents = fs::read_to_string("tests/valid_samples/{sample_path}").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options {{ optimize: false }});

    let Ok(output) = result else {{
        let Err(errors) = result else {{ unreachable!() }};

        for e in &errors {{
            e.print(&context);
        }}

        panic!("Compilation of sample \\"{stem}\\" resulted in {{}} error(s)", errors.len()); 
    }};

    assert_snapshot!("{stem}", output);
}}
"""

code = FILE_TEMPLATE.format("\n".join([FUNCTION_TEMPLATE.format(sample_path=f, stem=Path(f).stem) for f in files]))

print(code.replace('\r', ''))
