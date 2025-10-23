{ pkgs, ... }:

{
  packages = with pkgs; [
    git
    rust-analyzer
    rustfmt
    nodejs
    tree-sitter
  ];

  languages.rust.enable = true;
}
