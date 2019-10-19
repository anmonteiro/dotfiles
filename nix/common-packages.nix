pkgs:
let
  # The list of packages to be installed
  # This setup is mostly based on https://github.com/nmattia/homies
  homies = with pkgs;
    [
      # Customized packages
      nix-npm-install
      now.now
      esy.esy
      lumo.lumo-cljs
      openssl
      gmp
      zshrc
      libffi
      ffmpeg
      libpng

      # Nixpkgs index packages
      awscli
      bat
      curl
      exa
      fzf
      git
      go
      gnupg
      jq
      libev
      mpir
      neovim
      nodejs-12_x
      cacert
      opam
      pkgconfig
      procps
      silver-searcher
      tmux
      tree
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

  now = pkgs.callPackage ./now { inherit pkgs; };
  esy = pkgs.callPackage ./esy { inherit pkgs; };
  lumo = pkgs.callPackage ./lumo-cljs { inherit pkgs; };
  openssl = import ./openssl pkgs;
  ffmpeg = import ./ffmpeg pkgs;
  gmp = import ./gmp pkgs;
  libffi = import ./libffi pkgs;
  libpng = import ./libpng pkgs;
  zshrc = pkgs.callPackage ./zshrc { inherit pkgs; };

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
