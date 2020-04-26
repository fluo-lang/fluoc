use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lib::parser::parser::Parser;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parser test", |b| b.iter(||  Parser::new(black_box("tests/parser_test.fluo".to_string())).unwrap().parse()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
