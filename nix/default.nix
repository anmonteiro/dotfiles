let
  system = builtins.currentSystem;
  flake = (import
    (fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/b4a3401.tar.gz";
      sha256 = "1qc703yg0babixi6wshn5wm2kgl5y1drcswgszh4xxzbrwkk9sv7";
    })
    { src = ../.; }).defaultNix;
  pkgs = flake.legacyPackages.${system};

  inherit (pkgs) callPackage;

  commonPkgs = callPackage ./homies/common-packages.nix { };
  macOSPkgs = callPackage ./homies/macos-packages.nix pkgs;

in
commonPkgs ++ macOSPkgs
