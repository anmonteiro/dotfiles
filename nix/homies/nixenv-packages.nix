{
  callPackage,
  git-lfs,
  nixVersions,
}:

let
  zshrc = callPackage ./zshrc { };
in

[
  nixVersions.latest
  zshrc # Installed via `configuration.nix` with native support on Linux
  git-lfs
]
