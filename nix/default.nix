let
  # The (pinned) Nixpkgs where the original packages are sourced from
  pkgs = import <nixpkgs> {};

  commonPkgs = import ./common-packages.nix pkgs;
  macOSPkgs = import ./macos-packages.nix pkgs;

in
  commonPkgs ++ macOSPkgs
