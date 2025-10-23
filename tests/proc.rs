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

#[test]
fn test_stdout_from_redirection() {
    call_main(
        r#"
        this_host = stdout ((echo -n "sodium") > (sed "s/o/a/g"));
        public main = exec (echo ("pre" + (this_host) + "post"));
        "#,
        &[],
    )
    .assert()
    .success()
    .stdout("presadiumpost\n");
}

#[test]
fn test_stdout_redirection_chaining() {
    // Test stdout redirection chaining through multiple commands
    // Note: Using 'tr' instead of awk to avoid curly braces (which would require interpolation syntax)
    call_main(
        r#"
        this_host = stdout (echo "sodium") > (sed "s/o/a/g") > (tr -d "\n");
        public main = exec (echo ("pre" + (this_host) + "post"));
        "#,
        &[],
    )
    .assert()
    .success()
    .stdout("presadiumpost\n");
}

#[test]
fn test_exec_status_code() {
    call_main(
        r#"
        public main = exec (echo (exec (echo "foobar" > grep "bar")));
        "#,
        &[],
    )
    .assert()
    .stdout("foobar\n0\n")
    .code(0);

    call_main(
        r#"
        public main = exec (echo (exec (echo "foobar" > grep "baz")));
        "#,
        &[],
    )
    .assert()
    .stdout("1\n")
    .code(0);
}

#[test]
fn test_list_argument_int() {
    call_main(
        "public main $nums: [int] = exec (echo (add_all($nums)));",
        &["1,2,3,4,5"],
    )
    .assert()
    .success()
    .stdout("15\n");
}

#[test]
fn test_list_argument_str() {
    call_main(
        r#"public main $words: [str] = exec (echo (first($words)));"#,
        &["hello,world,test"],
    )
    .assert()
    .success()
    .stdout("hello\n");
}

#[test]
fn test_list_argument_empty() {
    call_main(
        "public main $nums: [int] = exec (echo (add_all($nums)));",
        &[""],
    )
    .assert()
    .success()
    .stdout("0\n");
}

#[test]
fn test_list_argument_with_spaces() {
    call_main(
        "public main $nums: [int] = exec (echo (add_all($nums)));",
        &["1, 2, 3"],
    )
    .assert()
    .success()
    .stdout("6\n");
}
