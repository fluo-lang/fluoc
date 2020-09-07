use lib::logger::{ErrorType, LoggerInner};
use lib::sourcemap::SourceMapInner;
use lib::typecheck::TypeCheckModule;

use std::path;
use std::rc::Rc;

macro_rules! set_up_typecheck {
    ($code: expr) => {{
        let filename = path::PathBuf::from("this_is_another_filename_test.fl");
        let sourcemap = SourceMapInner::new();
        let filename_id = sourcemap.borrow_mut().insert_file(
            filename,
            concat!("@[no_std]\n@[no_core]\n", $code).to_string(),
        );

        let logger = LoggerInner::new(true, Rc::clone(&sourcemap));

        TypeCheckModule::new(filename_id, logger, sourcemap)
    }};
}

macro_rules! assert_error {
    ($code: expr, $expected_error: expr, $name: ident) => {
        #[test]
        fn $name() {
            let mut typechecker = set_up_typecheck!($code);
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

macro_rules! assert_ok {
    ($code: expr, $name: ident) => {
        #[test]
        fn $name() {
            let mut typechecker = set_up_typecheck!($code);
            assert!(typechecker.type_check().is_ok());
        }
    };
}

/*

    assert_error!(
        r#"let entry = () -> () {
    x;
    return ();
};"#,
        vec![ErrorType::UndefinedSymbol],
        undef_var
    );

    assert_error!(
        r#"let entry = () {
    let x: i32;
    if false {
        x = 0;
    }
    let y: i32 = x;
};"#,
        vec![ErrorType::PossibleUninitVal],
        if_cond_no_else
    );

    assert_error!(
        r#"type Km = i32;
let entry = () {
    let x: Miles;
};"#,
        vec![ErrorType::UndefinedType],
        undef_type
    );

    assert_error!(
        r#"type Km = i32;
let entry = () {
    let x: i64 = (10 as Km) as i64;
};"#,
        vec![ErrorType::TypeCast],
        type_cast_error
    );*/

assert_error!(
    r#"let entry = () {
    return 1923;
};"#,
    vec![ErrorType::TypeMismatch],
    implicit_return_mismatch
);

assert_error!(
    r#"let entry = () -> i32 {
    return 1923 is i64;
};"#,
    vec![ErrorType::TypeMismatch],
    return_is_mismatch
);

assert_error!(
    r#"let entry = () -> () {
    return 1923;
};"#,
    vec![ErrorType::TypeMismatch],
    explicit_return_mismatch
);

assert_ok!(
    r#"let entry = () -> i32 {
    let x = 10;
    return x;
};"#,
    basic_i32_infer
);

assert_ok!(
    r#"let entry = () -> i32 {
    let y = 10;
    return x(y);
};

let x = (a: _) -> _ {
    return a;
};"#,
    identity_func_infer
);
