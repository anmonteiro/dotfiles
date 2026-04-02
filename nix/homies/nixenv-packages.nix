{
  callPackage,
  git-lfs,
  nixVersions,
}:

let
  zshrc = callPackage ./zshrc { };
in

[
  (nixVersions.latest.overrideAttrs (_: {
    doCheck = false;
  }))
  zshrc # Installed via `configuration.nix` with native support on Linux
  git-lfs
]
