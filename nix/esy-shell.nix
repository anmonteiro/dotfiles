{ pkgs ? import <nixpkgs> {} }:

# THIS IS AN EXAMPLE OF A NIX-SHELL THAT ALLOWS FOR REGULAR ESY DEVELOPMENT ON
# NIXOS (OR WITH PLAIN NIX ON ANOTHER SYSTEM.)

let
  # FIRST PULL IN THE EASY NIX BUILD SO WE HAVE ACCESS TO OUR BINARY ESY PACKAGE
  # (NOT CURRENTLY AVAILABLE IN THE STANDARD NIXPKGS REPO).
  esy = pkgs.callPackage (pkgs.callPackage ./homies/esy {}) {};

  # NOW LETS CREATE A CUSTOM ENVIRONMENT WITH ESY AND THE NATIVE OS TOOLS THAT
  # ESY EXPECTS ON ANY SYSTEM.
  esy-with-tools-fhs =
    pkgs.buildFHSUserEnv {
      name = "esy-with-tools-fhs";
      # HERE WE INCLUDE ALL DESIRED PACKAGES.  THIS MAY VARY DEPENDING ON THE
      # PROJECT. YOU MIGHT NEED `sqlite.dev` OR `curl.dev` FOR YOUR OWN PROJECT
      # EXAMPLE. THE PACKAGES BELOW ARE THE MINIMUM NEEDED TO BUILD ESY WITH
      # ESY.
      targetPkgs = pkgs: with pkgs; [
        binutils cacert curl esy gnumake m4 patch perl stdenv.cc which git pkg-config
      ];
      # THE BUILDFHSUSERENV DOSEN'T ALLOW FOR ENVIRONMENT VARIBLES.  SO WE'LL
      # USE AN ENV TO LAUNCH BASH INSTEAD OF LAUNCHING BASH DIRECLY (DEFAULT.)
      runScript = "env GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt bash";
    };

    in
# TO BUILD ESY TYPE `nix-shell --pure` AND THEN BUILD LIKE NORMAL `esy build`
esy-with-tools-fhs.env
