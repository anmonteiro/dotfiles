let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/58960d0229c2805c1affccdd58d010d824b63ecf.tar.gz;
    sha256 = "110d6jv7ghwa8ybxpl3szis6ilq970ppxbw9gz15crzi64kzdm4w";
  };

in
import overlays
