let
  # The (pinned) Nixpkgs where the original packages are sourced from
  pkgs = import ../../nix-overlays/boot.nix { };
  inherit (pkgs) callPackage;

  commonPkgs = callPackage ./homies/common-packages.nix { };
  macOSPkgs = callPackage ./homies/macos-packages.nix pkgs;

in
commonPkgs ++ macOSPkgs
