pkgs:
let
  macOSPkgs = with pkgs; [
    cachix
    gmp
    libffi
    libpng
    nix
    openssl
    pkgconfig
    vagrant
    yarn
    zshrc # Installed via `configuration.nix` with native support on Linux
  ];

  cachixSource = fetchTarball { url = "https://cachix.org/api/v1/install"; };

  cachix = (import cachixSource {}).cachix;
  zshrc = pkgs.callPackage ./zshrc { inherit pkgs; };
  lumo = pkgs.callPackage ./lumo-cljs { };

in macOSPkgs
