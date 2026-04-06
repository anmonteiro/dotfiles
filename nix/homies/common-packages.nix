{
  pkgs,
  callPackage,
  lib,
  stdenv,
}:

let
  # Kitty with a custom kitty.conf baked in
  kitty = callPackage ./kitty {
    inherit (pkgs.darwin) autoSignDarwinBinariesHook;
  };

  # Tmux with a custom tmux.conf baked in
  tmux = callPackage ./tmux { };
  neovimWithPython = pkgs.neovim.override {
    withPython3 = true;
    extraPython3Packages =
      ps: with ps; [
        pynvim
      ];
  };
  pythonWithPynvim = pkgs.python3.withPackages (
    ps: with ps; [
      pynvim
    ]
  );

in

with pkgs;
[
  bat
  cacert
  curl
  ocaml-ng.ocamlPackages_5_5.carl
  eza
  ffmpeg
  git
  gnupg
  htop
  jq
  nixd
  nixfmt
  neovimWithPython
  nix-zsh-completions
  procps
  pythonWithPynvim
  silver-searcher
  stylua
  fff
  fff-cli

  # for nvim
  ripgrep
  fd
  tree-sitter
  zsh
  terminal-notifier

  # Remote development
  devpod
  ty
  typescript-language-server
  nodejs_latest
  github-mcp-server
]
++ [
  kitty
  tmux
]
