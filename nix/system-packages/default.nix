pkgs: with pkgs;
let
  packages = [
    gcc
    gnumake
    binutils

    dmenu
    xlibs.xmodmap
    xmobar

    htop

    brave
    discord
  ];

  xmobar = import ./xmobar (with pkgs;
    { inherit
        makeWrapper
        symlinkJoin;
      xmobar = haskellPackages.xmobar;
    });


in
  packages
