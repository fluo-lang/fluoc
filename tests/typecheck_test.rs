#[cfg(test)]
mod typecheck_tests {
    use lib::logger::{ErrorType, LoggerInner};
    use lib::typecheck::TypeCheckModule;
    use lib::sourcemap::SourceMapInner;

    use std::path;
    use std::rc::Rc;

    macro_rules! assert_error {
        ($code: expr, $expected_error: expr, $name: ident) => {
            #[test]
            fn $name() {
                let filename = path::PathBuf::from("this_is_another_filename_test.fl");
                let mut sourcemap = SourceMapInner::new();
                let filename_id = sourcemap.borrow_mut().insert_file(filename, concat!("@[no_std]\n@[no_core]\n", $code).to_string());

                let logger = LoggerInner::new(true, Rc::clone(&sourcemap));

                let mut typechecker = TypeCheckModule::new(
                    filename_id,
                    logger,
                    sourcemap,
                );
                assert_eq!(
                    typechecker
                        .type_check()
                        .expect_err("Failed to typecheck")
                        .into_iter()
                        .map(|err| err.error)
                        .collect::<Vec<_>>(),
                    $expected_error
                );
            }
        };
    }

    assert_error!(
        r#"def entry() {
    let x: int;
    if false {
        x = 0;
    }
    let y: int = x;
}"#,
        vec![ErrorType::PossibleUninitVal],
        if_cond_no_else
    );
}
