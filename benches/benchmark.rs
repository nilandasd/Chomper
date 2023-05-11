use chomper::Chomper;
use criterion::{criterion_group, criterion_main, Criterion};

fn float_compare() {
    let chomper = Chomper::from(r#"-?\d+(\.\d+)?"#);

    for _ in 0..10000 {
        assert!(chomper.compare("654321"));
        assert!(chomper.compare("0.0"));
        assert!(chomper.compare("123.456"));
        assert!(chomper.compare("-123.456"));
        assert!(chomper.compare("0.420420420420420420"));

        assert!(!chomper.compare("should not match"));
        assert!(!chomper.compare("12414..123123"));
        assert!(!chomper.compare("1214."));
        assert!(!chomper.compare("-1214."));
        assert!(!chomper.compare("-."));
        assert!(!chomper.compare("1214.12412."));
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("float_compare", |b| b.iter(|| float_compare()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
