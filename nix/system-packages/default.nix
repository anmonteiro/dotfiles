{ pkgs }:
with pkgs;
let
  packages = [
    gcc
    gnumake
    binutils

    dmenu
    xlibs.xmodmap
    xmobar

    htop
    unrar
    mpv

    brave
    discord
    maim # Screenshots
    xfce4-14.thunar
    xfce4-14.thunar-volman
    xfce4-14.xfce4-icon-theme
    xfce.gvfs
  ];

  xmobar = import ./xmobar ({
    inherit
    makeWrapper
    symlinkJoin;
    xmobar = haskellPackages.xmobar;
  });

in
  packages
