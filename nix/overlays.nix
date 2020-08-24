
let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/ddc82904.tar.gz;
    sha256 = "0ngbg6q64gswvqbbsgwgyxy470a88bxy5b2m4zn611cpzqn5ih1z";
  };

in import overlays

