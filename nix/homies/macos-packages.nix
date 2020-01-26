pkgs:
let
  macOSPkgs = with pkgs; [
    aws-iam-authenticator
    cachix
    esy
    git-lfs
    gmp
    kubernetes
    libffi
    libpng
    nix
    opam
    openssl
    pkgconfig
    python
    pythonPackages.pywatchman
    terraform_0_12
    vagrant
    watchman
    yarn
    zshrc # Installed via `configuration.nix` with native support on Linux
  ];

  cachixSource = fetchTarball { url = "https://cachix.org/api/v1/install"; };

  cachix = (import cachixSource {}).cachix;
  zshrc = pkgs.callPackage ./zshrc { inherit pkgs; };
  esy = pkgs.callPackage (pkgs.callPackage ./esy { }) {
    githubInfo = {
      owner = "esy";
      rev    = "95f9244";
      sha256 = "128qbjad9583dssgw1mrpshbd9w6armjjg933pkhi0xaa7v0crqg";
    };
  };
  lumo = pkgs.callPackage ./lumo-cljs { };

in macOSPkgs
