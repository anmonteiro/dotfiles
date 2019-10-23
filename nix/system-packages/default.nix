pkgs: with pkgs;
let
  packages = [
    gcc
    gnumake
    binutils

    dmenu
    xlibs.xmodmap
    xmobar

    brave
  ];

  xmobar = import ./xmobar (with pkgs;
    { inherit
        makeWrapper
        symlinkJoin;
      xmobar = haskellPackages.xmobar;
    });


in
  packages
