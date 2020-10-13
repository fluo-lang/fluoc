use criterion::Criterion;
use lib::logger::LoggerInner;
use lib::sourcemap::SourceMapInner;
use lib::typecheck;

use std::path;
use std::rc::Rc;

pub fn typecheck_benchmark(c: &mut Criterion) {
    let sourcemap = SourceMapInner::new();
    let filename_id = sourcemap.borrow_mut().insert_file(
        path::PathBuf::from(
            "my_long_filename_test_this_is_really_long_but_its_for_a_Test_so_who_cares.fl",
        ),
        r#"let entry = () -> (i32, i32) {
    let y = 11;
    let y = y;
    let my_func = () -> _ {
        return y;
    };
    return (x(x(y)), my_func());
};

let x = (a: _) -> _ {
    return y(y(y(y(a))));
};

let y = (a: _) -> _ {
    return z(z(z(z(a))));
};

let z = (a: _) -> _ {
    return p(p(p(p(a))));
};

let p = (a: _) -> _ {
    return l(l(l(l(a))));
};

let l = (a: _) -> _ {
    return a;
};
        "#
        .to_string(),
    );

    let logger = LoggerInner::new(true, Rc::clone(&sourcemap));
    logger.borrow_mut().disable();

    c.bench_function("typecheck complex", |b| {
        b.iter(|| {
            let mut typechecker = typecheck::TypeCheckModule::new(
                filename_id,
                Rc::clone(&logger),
                Rc::clone(&sourcemap),
            );
            typechecker.type_check().unwrap();
        })
    });
}
