let
  # The (pinned) Nixpkgs where the original packages are sourced from
  pkgs = import <nixpkgs> {
    overlays = [ (import ./overlays.nix) ];
  };

  commonPkgs = import ./homies/common-packages.nix { inherit pkgs; };
  macOSPkgs = import ./homies/macos-packages.nix pkgs;

in
  commonPkgs ++ macOSPkgs
