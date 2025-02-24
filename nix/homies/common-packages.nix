{ pkgs, callPackage, lib, stdenv }:

let
  # Kitty with a custom kitty.conf baked in
  kitty = callPackage ./kitty {
    inherit (pkgs.darwin) autoSignDarwinBinariesHook;
  };

  # Tmux with a custom tmux.conf baked in
  tmux = callPackage ./tmux { };

in

with pkgs; [
  bat
  cacert
  curl
  ocaml-ng.ocamlPackages_5_3.carl
  eza
  ffmpeg
  fzf
  git
  gnupg
  htop
  jq
  nixpkgs-fmt
  neovim
  nix-zsh-completions
  procps
  silver-searcher
  stylua

  # for nvim
  ripgrep
  fd
  zsh

  # Remote development
  devpod
] ++ [
  kitty
  tmux
]
