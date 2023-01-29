mod common;

use common::call_main;

#[test]
fn test_proc() {
    call_main("public main = exec (echo Hello World);", &[])
        .assert()
        .success()
        .stdout("Hello World\n");
}

#[test]
fn test_stdout_redirection() {
    call_main("public main = exec (echo Hello World > grep World);", &[])
        .assert()
        .success()
        .stdout("Hello World\n");

    call_main("public main = exec (echo Hello People > grep World);", &[])
        .assert()
        .success()
        .stdout("");
}

#[test]
fn test_arguments() {
    // FIXME: make it so that this doesn't need parenthesis
    call_main(
        "public main $p: str = exec ((echo $p) > grep World);",
        &["Hello World"],
    )
    .assert()
    .success()
    .stdout("Hello World\n");

    call_main(
        "public main $p: str = exec ((echo $p) > grep World);",
        &["Hello People"],
    )
    .assert()
    .success()
    .stdout("");
}

#[test]
fn test_seq() {
    call_main(
        "public main = seq [
            echo foo;
            echo bar;
        ];",
        &[],
    )
    .assert()
    .success()
    .stdout("foo\nbar\n");
}
