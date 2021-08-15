pkgs:
let
  cachixSource = fetchTarball { url = "https://cachix.org/api/v1/install"; };

  cachix = (import cachixSource { }).cachix;
  zshrc = pkgs.callPackage ./zshrc { inherit pkgs; };
  lumo = pkgs.callPackage ./lumo-cljs { };

in

with pkgs; [
  cachix
  nix
  # vagrant
  zshrc # Installed via `configuration.nix` with native support on Linux
  git-lfs
]
