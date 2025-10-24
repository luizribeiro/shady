use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::io::Write;
use std::process::{Command, Stdio};

/// Helper function to run a shady script via stdin
fn run_shady_script(script: &str, args: &[&str]) -> std::process::Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_shady"));
    cmd.arg("-");
    cmd.arg("main");
    cmd.args(args);
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    let mut child = cmd.spawn().expect("Failed to spawn shady");
    child
        .stdin
        .as_mut()
        .expect("Failed to open stdin")
        .write_all(script.as_bytes())
        .expect("Failed to write to stdin");

    child.wait_with_output().expect("Failed to wait for child")
}

/// Benchmark: Single pipe operation (echo > grep)
fn bench_single_pipe(c: &mut Criterion) {
    c.bench_function("single_pipe", |b| {
        b.iter(|| {
            let script = "public main = exec (echo Hello World > grep World);";
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });
}

/// Benchmark: Chained pipes (echo > sed > awk)
fn bench_chained_pipes(c: &mut Criterion) {
    let mut group = c.benchmark_group("chained_pipes");

    // 2 pipes
    group.bench_function(BenchmarkId::new("pipes", 2), |b| {
        b.iter(|| {
            let script = r#"public main = exec (echo "sodium" > sed "s/o/a/g");"#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    // 3 pipes
    group.bench_function(BenchmarkId::new("pipes", 3), |b| {
        b.iter(|| {
            let script =
                r#"public main = exec (echo "sodium" > sed "s/o/a/g" > awk '{printf $0}');"#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    // 4 pipes
    group.bench_function(BenchmarkId::new("pipes", 4), |b| {
        b.iter(|| {
            let script =
                r#"public main = exec (echo "sodium" > sed "s/o/a/g" > awk '{printf $0}' > cat);"#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    group.finish();
}

/// Benchmark: Stdout capture from pipes
fn bench_stdout_capture(c: &mut Criterion) {
    c.bench_function("stdout_capture", |b| {
        b.iter(|| {
            let script = r#"
                result = stdout ((echo "sodium") > (sed "s/o/a/g"));
                public main = exec (echo (result));
            "#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });
}

/// Benchmark: Stdout capture with string concatenation
fn bench_stdout_capture_with_concat(c: &mut Criterion) {
    c.bench_function("stdout_capture_concat", |b| {
        b.iter(|| {
            let script = r#"
                this_host = stdout ((echo "sodium") > (sed "s/o/a/g"));
                public main = exec (echo ("pre" + (this_host) + "post"));
            "#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });
}

/// Benchmark: Sequential command execution
fn bench_seq_commands(c: &mut Criterion) {
    let mut group = c.benchmark_group("seq_commands");

    // 2 commands
    group.bench_function(BenchmarkId::new("commands", 2), |b| {
        b.iter(|| {
            let script = r#"
                public main = seq [
                    echo foo;
                    echo bar;
                ];
            "#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    // 5 commands
    group.bench_function(BenchmarkId::new("commands", 5), |b| {
        b.iter(|| {
            let script = r#"
                public main = seq [
                    echo foo;
                    echo bar;
                    echo baz;
                    echo qux;
                    echo quux;
                ];
            "#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    // 10 commands
    group.bench_function(BenchmarkId::new("commands", 10), |b| {
        b.iter(|| {
            let script = r#"
                public main = seq [
                    echo 1; echo 2; echo 3; echo 4; echo 5;
                    echo 6; echo 7; echo 8; echo 9; echo 10;
                ];
            "#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    group.finish();
}

/// Benchmark: Data throughput - varying amounts of data through pipes
fn bench_data_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("data_throughput");

    // Small data (10 lines)
    group.bench_function(BenchmarkId::new("lines", 10), |b| {
        b.iter(|| {
            let script = r#"public main = exec (yes hello > head -10 > cat);"#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    // Medium data (100 lines)
    group.bench_function(BenchmarkId::new("lines", 100), |b| {
        b.iter(|| {
            let script = r#"public main = exec (yes hello > head -100 > cat);"#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    // Large data (1000 lines)
    group.bench_function(BenchmarkId::new("lines", 1000), |b| {
        b.iter(|| {
            let script = r#"public main = exec (yes hello > head -1000 > cat);"#;
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });

    group.finish();
}

/// Benchmark: Process spawning overhead
fn bench_process_spawn(c: &mut Criterion) {
    c.bench_function("process_spawn", |b| {
        b.iter(|| {
            let script = "public main = exec (echo Hello);";
            let output = run_shady_script(black_box(script), &[]);
            assert!(output.status.success());
        });
    });
}

criterion_group!(
    benches,
    bench_single_pipe,
    bench_chained_pipes,
    bench_stdout_capture,
    bench_stdout_capture_with_concat,
    bench_seq_commands,
    bench_data_throughput,
    bench_process_spawn,
);

criterion_main!(benches);
