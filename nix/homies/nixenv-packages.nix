pkgs:
let
  zshrc = pkgs.callPackage ./zshrc { inherit pkgs; };
  lumo = pkgs.callPackage ./lumo-cljs { };

in

with pkgs; [
  nix
  # vagrant
  zshrc # Installed via `configuration.nix` with native support on Linux
  git-lfs
]
++ lib.optionals pkgs.stdenv.isLinux [ xsel ]
