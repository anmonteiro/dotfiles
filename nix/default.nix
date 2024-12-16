let
  system = builtins.currentSystem;
  flake = (import
    (fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/ff81ac966.tar.gz";
      sha256 = "19d2z6xsvpxm184m41qrpi1bplilwipgnzv9jy17fgw421785q1m";
    })
    { src = ./..; }
  ).defaultNix;

  pkgs = flake.legacyPackages.${system};

  commonPkgs = pkgs.callPackage ./homies/common-packages.nix { };
  nixEnvPkgs = pkgs.callPackage ./homies/nixenv-packages.nix { };

in
commonPkgs ++ nixEnvPkgs
