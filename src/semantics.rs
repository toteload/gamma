use crate::ast;
use std::collections::HashSet;

pub fn has_main_function(items: &[ast::Item]) -> bool {
    for item in items {
        match item {
            ast::Item::Function { name, .. } if name == "main" => return true,
            _ => (),
        }
    }

    false
}

pub fn do_all_called_functions_exist(items: &[ast::Item]) -> bool {
    let defined_functions = items
        .iter()
        .map(|item| match item {
            ast::Item::Function { name, .. } => name,
        })
        .collect::<HashSet<_>>();

    todo!();
}

pub fn do_all_referenced_parameters_exist(items: &[ast::Item]) -> bool {
    todo!();
}

pub fn is_semantically_correct(items: &[ast::Item]) -> bool {
    todo!()
}
