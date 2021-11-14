{
  tarball = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/c9e9b33c4.tar.gz;
    sha256 = "1s6vw8h7jgiszs27amnbqvilvx15hg3r2mb65r5xq0drrigmhz9h";
  };

  local = /home/anmonteiro/projects/nix-overlays;
}.tarball
