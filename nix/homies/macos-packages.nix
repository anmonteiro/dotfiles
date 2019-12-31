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
    zshrc
  ];

  zshrc = pkgs.callPackage ./zshrc { inherit pkgs; };

in macOSPkgs
