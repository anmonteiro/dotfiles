let
  system = builtins.currentSystem;
  flake = (import
    (
      fetchTarball {
        url = https://github.com/edolstra/flake-compat/archive/0f9255e01.tar.gz;
        sha256 = "0m9grvfsbwmvgwaxvdzv6cmyvjnlww004gfxjvcl806ndqaxzy4j";
      }
    )
    { src = ./..; }
  ).defaultNix;

  pkgs = flake.legacyPackages.${system};

  inherit (pkgs) callPackage;

  commonPkgs = callPackage ./homies/common-packages.nix { };
  nixEnvPkgs = callPackage ./homies/nixenv-packages.nix pkgs;

in
commonPkgs ++ nixEnvPkgs
