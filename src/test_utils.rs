use assert_cmd::Command;

pub fn call_main(script: &str, args: &[&str]) -> Command {
    let mut cmd = Command::cargo_bin("shady").unwrap();
    cmd.write_stdin(script);
    cmd.arg("-");
    cmd.arg("main");
    cmd.args(args);
    cmd
}
