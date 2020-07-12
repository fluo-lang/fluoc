#[cfg(test)]
mod typecheck_tests {
    use lib::logger::logger::{ ErrorType, Logger };
    use lib::typecheck::typecheck::TypeCheckModule;
    use std::path;
    use std::rc::Rc;
    use std::cell::RefCell;

    macro_rules! assert_error {
        ($code: expr, $expected_error: expr, $name: ident) => {
            #[test]
            fn $name() {
                let logger = Rc::new(RefCell::new(Logger::new(true)));
                let filename = path::Path::new("this_is_another_filename_test.fl");
                let mut typechecker = TypeCheckModule::new(filename, concat!("@[no_std]\n@[no_core]\n", $code), logger);
                println!("{:?}", typechecker.type_check());
                //assert_eq!(typechecker.type_check().expect_err("Failed to typecheck").into_iter().map(|err| err.error).collect::<Vec<_>>(), $expected_error);
            }
        }
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

