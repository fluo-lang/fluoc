mod parser_bench;
mod typecheck_bench;

use criterion::{criterion_group, criterion_main, Criterion};

criterion_group! {
    name = parser_bench;
    config = Criterion::default().sample_size(10);
    targets = parser_bench::parser_benchmark
}

criterion_group!(typecheck_bench, typecheck_bench::typecheck_benchmark);

criterion_main!(parser_bench, typecheck_bench);
