let
  # The (pinned) Nixpkgs where the original packages are sourced from
  src = import ./overlays.nix;
  pkgs = import "${src}/boot.nix" { };
  inherit (pkgs) callPackage;

  commonPkgs = callPackage ./homies/common-packages.nix { };
  macOSPkgs = callPackage ./homies/macos-packages.nix pkgs;

in
commonPkgs ++ macOSPkgs
