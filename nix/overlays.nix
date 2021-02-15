let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/de5512e.tar.gz;
    sha256 = "1g1g4ys6wmrjbdssqdah43yxq3hd4fvkphlmb1yspn87xzxi98k7";
  };

in
import overlays
