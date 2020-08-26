use criterion::{criterion_group, criterion_main, Criterion};
use lib::logger::LoggerInner;
use lib::parser;
use lib::sourcemap::SourceMapInner;

use std::path;
use std::rc::Rc;

#[allow(unused_must_use)]
fn criterion_benchmark(c: &mut Criterion) {
    let sourcemap = SourceMapInner::new();
    sourcemap.borrow_mut().insert_file(
        path::PathBuf::from(
            "my_long_filename_test_this_is_really_long_but_its_for_a_Test_so_who_cares.fl",
        ),
        r#"/*	
hlleoa1234567890qwertyuiopasdfghjklzxcvbnm,./?><;'":[]\|}{=-0987654321`~!@#$%^&*()_+	
one two	
djawd	
sfghsdjajdksajfiwjijfa	
*/	

def other() {	
    def other_1() -> () {	
        return 10;	
    }	
    let x: int = 10;	

    def other_1() -> int {	
        return 10;	
    }

    let _qwertyuiopasdfghjklzxcvbnm: int;	
    _qwertyuiopasdfghjklzxcvbnm = "hi";	
    hi(10, 102, _qwertyuiopasdfghjklzxcvbnm+"other hi", x);	
    return (((((((1023), (((123))), x = 10, (((let x: int = 10))), (("awd"))))))));
    (((((((((((((())))))))))))));
}
"#
        .to_string(),
    );

    let logger = LoggerInner::new(true, Rc::clone(&sourcemap));

    c.bench_function("parser simple", |b| {
        b.iter(|| {
            let mut my_parser =
                parser::parser::Parser::new(0, Rc::clone(&logger), Rc::clone(&sourcemap));
            my_parser.initialize_expr();
            my_parser.parse();
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = criterion_benchmark
}

criterion_main!(benches);
