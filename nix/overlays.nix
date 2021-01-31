
let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/50a9075.tar.gz;
    sha256 = "0akg5qbpcyr51ck3pd0wi23z9y59jpsjis23v9yqq5qdmn47d1by";
  };

in import overlays

