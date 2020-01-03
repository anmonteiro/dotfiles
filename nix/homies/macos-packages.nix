pkgs:
let
  macOSPkgs = with pkgs; [
    aws-iam-authenticator
    git-lfs
    kubernetes
    nix
    opam
    pkgconfig
    python
    pythonPackages.pywatchman
    terraform_0_12
    vagrant
    watchman
    zshrc # Installed via `configuration.nix` with native support on Linux

    esy

    openssl
    gmp
    libffi
    libpng
  ];

  zshrc = pkgs.callPackage ./zshrc { inherit pkgs; };
  esy = pkgs.callPackage (pkgs.callPackage ./esy { }) {
    githubInfo = {
      owner = "esy";
      rev    = "95f9244";
      sha256 = "128qbjad9583dssgw1mrpshbd9w6armjjg933pkhi0xaa7v0crqg";
    };
  };

in macOSPkgs
