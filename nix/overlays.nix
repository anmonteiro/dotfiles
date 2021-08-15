let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/146c144.tar.gz;
    sha256 = "1wm25vhnf2dcd5am0pagxxf62jknqlr2kcvbfhsz7vcn5v711hnr";
  };

in
import overlays
