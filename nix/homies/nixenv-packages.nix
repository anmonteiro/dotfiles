pkgs:

let
  inherit (pkgs) callPackage git-lfs nix;
  zshrc = callPackage ./zshrc { inherit pkgs; };
  lumo = callPackage ./lumo-cljs { };

in

[
  nix
  zshrc # Installed via `configuration.nix` with native support on Linux
  git-lfs
]
