
let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/23a7d1f.tar.gz;
    sha256 = "1dp39gb2y53521xqz6hny5ydflqcg3jxzi04b0ql1jxpvbmnapmg";
  };

in import overlays

