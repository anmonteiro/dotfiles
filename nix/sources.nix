{
  tarball = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/07ff41ac9.tar.gz;
    sha256 = "0k3vcq15raj75z9khijhwfv0ppzw9msm64dnb9spk05prvvaaixr";
  };

  local = /home/anmonteiro/projects/nix-overlays;
}.tarball
