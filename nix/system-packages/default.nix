{ pkgs }:
with pkgs;
let
  xmobar = import ./xmobar ({
    inherit
      makeWrapper
      symlinkJoin;
    xmobar = haskellPackages.xmobar;
  });

  packages = [
    gcc
    gnumake
    binutils
    tree

    dmenu
    xorg.xmodmap
    xmobar
    xclip
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

in
packages
