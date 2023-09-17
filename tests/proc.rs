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
fn test_default_arguments() {
    call_main(r#"public main $p: str ("Hi There") = exec (echo $p);"#, &[])
        .assert()
        .success()
        .stdout("Hi There\n");
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

#[test]
fn test_string_concat() {
    call_main(
        r#"public main = exec (echo ("a" + (stdout (echo -n "b")) + "c"));"#,
        &[],
    )
    .assert()
    .success()
    .stdout("abc\n");
}

#[test]
fn test_string_concat_simpler() {
    call_main(
        r#"public main = exec (echo ("a" + (stdout (echo -n "b"))));"#,
        &[],
    )
    .assert()
    .success()
    .stdout("ab\n");
}
