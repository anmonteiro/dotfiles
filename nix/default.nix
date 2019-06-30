let
  # The (pinned) Nixpkgs where the original packages are sourced from
  pkgs = import <nixpkgs> {};

  # The list of packages to be installed
  # This setup is mostly based on https://github.com/nmattia/homies
  homies = with pkgs;
    [
      # Customized packages
      nix-npm-install
      now
      esy
      lumo
      openssl
      gmp
      zshrc
      libffi
      ffmpeg
      libpng

      # Nixpkgs index packages
      autoconf
      awscli
      bat
      boot
      curl
      emacs
      exa
      fzf
      git
      git-lfs
      go
      gnupg
      jq
      kubernetes
      libev
      mongodb
      mpir
      nghttp2
      neovim
      nodejs-12_x
      cacert
      nix
      opam
      pkgconfig
      python
      pythonPackages.pywatchman
      procps
      rlwrap
      rustup
      silver-searcher
      terraform
      tmux
      tree
      vagrant
      watchman
      wget
      yarn
      zsh
      oh-my-zsh
      zsh-completions
      zsh-syntax-highlighting
      zsh-history-substring-search
      zsh-autosuggestions
      nix-zsh-completions
    ];

  ## Some customizations

  # A custom `nix-npm-install` (see `nix-npm-install/defaul.nix` for details)
  # command to install npm packages in the nix sandbox:
  nix-npm-install = pkgs.callPackage ./nix-npm-install {};

  now = pkgs.callPackage ./now {};
  esy = pkgs.callPackage ./esy {};
  lumo = pkgs.callPackage ./lumo-cljs {};
  bs-platform = pkgs.callPackage ./bs-platform {};
  openssl = import ./openssl { inherit pkgs; };
  ffmpeg = import ./ffmpeg { inherit pkgs; };
  gmp = import ./gmp { inherit pkgs; };
  libffi = import ./libffi { inherit pkgs; };
  libpng = import ./libpng { inherit pkgs; };
  zshrc = pkgs.callPackage ./zshrc {};

  # Git with config baked in
  #git = import ./git (
  #  { inherit (pkgs) makeWrapper symlinkJoin;
  #    git = pkgs.git;
  #  });

  # Tmux with a custom tmux.conf baked in
  #tmux = import ./tmux (with pkgs;
  #  { inherit
  #      makeWrapper
  #      symlinkJoin
  #      writeText
  #      ;
  #    tmux = pkgs.tmux;
  #  });

  #snack = (import (fetch "snack")).snack-exe;

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
