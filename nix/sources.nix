{
  tarball = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/4a3392d.tar.gz;
    sha256 = "1n943k9x5936dgc25d56l6y3j6fkgmfxmvwb3jsb2kmm21gqs6rr";
  };

  local = /home/anmonteiro/projects/nix-overlays;
}.tarball
