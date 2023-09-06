use std::fs;

#[test]
fn test_sample_programs() {
    const SAMPLE_FOLDER: &str = "tests/samples/";

    let sample_paths = fs::read_dir(SAMPLE_FOLDER).unwrap().filter_map(|entry| {
        let Ok(entry) = entry else { return None; };
        let path = entry.path();

        if !path.is_file() {
            return None;
        }

        let Some(ext) = path.extension() else { return None; };

        if ext != "gamma" {
            return None;
        }

        Some(path)
    }).collect::<Vec<_>>();

    for path in sample_paths {
        let contents = fs::read_to_string(path).unwrap();

        todo!("Try to compile the program and compare the output with the snapshot");
    }
}
