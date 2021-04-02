let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/19c213e.tar.gz;
    sha256 = "09a6c6lgc4djvarskakk07rs092l5zl9qk76rgl9pi1666wajrz4";
  };

in
import overlays
