
let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/1e18d3b.tar.gz;
    sha256 = "02jl1vdyili1yihl9zmc23snvmka202ll2ch6fwksn4fms1p28kw";
  };

in import overlays

