{ pkgs, lib, stdenv }:
let
  # The list of packages to be installed
  # This setup is mostly based on https://github.com/nmattia/homies
  inherit (pkgs) kitty darwin callPackage;

  # Kitty with a custom kitty.conf baked in
  kittyWrapped = callPackage ./kitty { };

  ## Some customizations

  # A custom `nix-npm-install` (see `nix-npm-install/defaul.nix` for details)
  # command to install npm packages in the nix sandbox:
  nix-npm-install = callPackage ./nix-npm-install { };

  ffmpeg = callPackage ./ffmpeg { };

  # Git with config baked in
  #git = import ./git (
  #  { inherit (pkgs) makeWrapper symlinkJoin;
  #    git = pkgs.git;
  #  });

  # Tmux with a custom tmux.conf baked in
  tmux = callPackage ./tmux { };

  # Neovim with a custom configuration baked in the derivation
  neovim = callPackage ./nvim { };
in

with pkgs; [
  # Nixpkgs index packages
  bat
  cacert
  curl
  ocaml-ng.ocamlPackages_5_3.carl
  eza
  fzf
  git
  gnupg
  htop
  jq
  nixpkgs-fmt
  nix-zsh-completions
  procps
  silver-searcher

  # for nvim
  ripgrep
  fd
  zsh

  # Remote development
  devpod
] ++ [
  ffmpeg
  kittyWrapped
  neovim
  tmux
]
