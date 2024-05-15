{ pkgs, lib, stdenv, linux-user, config ? null }:
let
  # The list of packages to be installed
  # This setup is mostly based on https://github.com/nmattia/homies
  inherit (pkgs) kitty darwin callPackage;

  # Kitty with a custom kitty.conf baked in
  kittyWrapped = callPackage ./kitty { };

  homies = with pkgs;
    [
      # Customized packages
      nix-npm-install
      ffmpeg

      # Nixpkgs index packages
      bat
      cacert
      curl
      ocaml-ng.ocamlPackages_5_2.carl
      eza
      fzf
      git
      gnupg
      htop
      jq
      neovim
      nixpkgs-fmt
      nix-zsh-completions
      procps
      silver-searcher
      tmux
      zsh

      # for nvim
      ripgrep
      fd

      # GUIs
      kittyWrapped
    ];

  ## Some customizations

  # A custom `nix-npm-install` (see `nix-npm-install/defaul.nix` for details)
  # command to install npm packages in the nix sandbox:
  nix-npm-install = pkgs.callPackage ./nix-npm-install { };

  openssl = import ./openssl pkgs;
  ffmpeg = import ./ffmpeg pkgs;
  gmp = import ./gmp pkgs;
  libffi = import ./libffi pkgs;
  libpng = import ./libpng pkgs;

  # Git with config baked in
  #git = import ./git (
  #  { inherit (pkgs) makeWrapper symlinkJoin;
  #    git = pkgs.git;
  #  });

  # Tmux with a custom tmux.conf baked in
  tmux = import ./tmux (with pkgs;
    {
      inherit
        makeWrapper
        symlinkJoin;
      tmux = pkgs.tmux;
    });

  # Neovim with a custom configuration baked in the derivation
  neovim = import ./nvim (with pkgs;
    {
      inherit
        makeWrapper
        fetchurl
        symlinkJoin
        copyPathToStore
        fzf
        git
        cacert
        stdenv;
      neovim = pkgs.neovim;
      inherit config linux-user;
    });


  # Vim with a custom vimrc and set of packages
  #vim = import ./vim (with pkgs;
  #  {inherit
  #      symlinkJoin
  #      makeWrapper
  #      vim_configurable
  #      vimUtils
  #      vimPlugins
  #      haskellPackages;
  #  });

in
#if pkgs.lib.inNixShell
  #then pkgs.mkShell
  #  { buildInputs = homies;
  #    shellHook = ''
  #      $(bashrc)
  #      '';
  #  }
  #else homies
homies
