{ pkgs, lib, stdenv, config ? null }:
let
  # The list of packages to be installed
  # This setup is mostly based on https://github.com/nmattia/homies
  inherit (pkgs) kitty darwin librsync callPackage;

  kittyPatched = kitty.overrideAttrs (o: {
    src = builtins.fetchurl {
      url = https://github.com/kovidgoyal/kitty/archive/f17d71454af7e704c9fff5059a2b9dd4f5653b79.tar.gz;
      sha256 = "1rzf7qja2yn5gndz76hwqqrha6fnxgjidlhx58aaqg9w698m6rya";
    };
    buildInputs = o.buildInputs ++
      lib.optionals stdenv.isDarwin [ librsync ] ++
      lib.optionals (stdenv.isDarwin && (builtins.hasAttr "UserNotifications" darwin.apple_sdk.frameworks)) [ darwin.apple_sdk.frameworks.UserNotifications ];
  });
  # Kitty with a custom kitty.conf baked in
  kittyWrapped = callPackage ./kitty { kitty = kittyPatched; };

  homies = with pkgs;
    [
      # Customized packages
      nix-npm-install
      ffmpeg

      # Nixpkgs index packages
      bat
      cacert
      curl
      ocamlPackages.carl
      exa
      fzf
      git
      gnupg
      htop
      jq
      neovim
      nixpkgs-fmt
      nix-zsh-completions
      nodejs-14_x
      procps
      silver-searcher
      tmux
      zsh

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
      inherit config;
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
