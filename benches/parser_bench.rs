use criterion::Criterion;
use lib::logger::LoggerInner;
use lib::parser;
use lib::sourcemap::SourceMapInner;

use std::path;
use std::rc::Rc;

pub fn parser_benchmark(c: &mut Criterion) {
    let sourcemap = SourceMapInner::new();
    let filename_id = sourcemap.borrow_mut().insert_file(
        path::PathBuf::from(
            "my_long_filename_test_this_is_really_long_but_its_for_a_Test_so_who_cares.fl",
        ),
        r#"/*	
hlleoa1234567890qwertyuiopasdfghjklzxcvbnm,./?><;'":[]\|}{=-0987654321`~!@#$%^&*()_+	
one two	
djawd	
sfghsdjajdksajfiwjijfa	
*/	

let other = () {	
    let other_1 = () -> () {	
        return 10;	
    };	
    let x: int = 10;

    let other_1 = () -> int {	
        return 10;	
    };

    let _qwertyuiopasdfghjklzxcvbnm: int =10;	
    hi(10, 102, _qwertyuiopasdfghjklzxcvbnm+"other hi", x);	
    return (((((((1023), (((123))), (((let x: int = 10))), (("awd"))))))));
    (((((((((a((a(((hi82))))))))))))));
};
"#
        .to_string(),
    );

    let logger = LoggerInner::new(true, Rc::clone(&sourcemap));

    c.bench_function("parser simple", |b| {
        b.iter(|| {
            let mut parser =
                parser::parser::Parser::new(filename_id, Rc::clone(&logger), Rc::clone(&sourcemap));
            parser.initialize_expr();
            parser.parse().unwrap();
        })
    });
}

