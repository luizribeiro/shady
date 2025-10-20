{ pkgs, ... }:

{
  packages = with pkgs; [
    git
    rust-analyzer
    rustfmt
  ];

  languages.rust.enable = true;
}
