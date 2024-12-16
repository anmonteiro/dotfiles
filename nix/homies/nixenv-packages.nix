{ callPackage, git-lfs, nix }:

let
  zshrc = callPackage ./zshrc { };
in

[
  nix
  zshrc # Installed via `configuration.nix` with native support on Linux
  git-lfs
]
