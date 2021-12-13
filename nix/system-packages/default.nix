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
    pulsemixer

    pciutils
    htop
    unrar
    mpv
    vlc
    pinentry-curses
    wireshark
    zoom-us

    brave
    discord
    maim # Screenshots
    xfce4-14.thunar
    xfce4-14.thunar-volman
    xfce4-14.xfce4-icon-theme
  ];

  xmobar = import ./xmobar ({
    inherit
      makeWrapper
      symlinkJoin;
    xmobar = haskellPackages.xmobar;
  });

in
packages
