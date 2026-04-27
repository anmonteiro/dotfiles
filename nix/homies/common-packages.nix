{
  pkgs,
  callPackage,
}:

let
  # Kitty with a custom kitty.conf baked in
  kitty = callPackage ./kitty {
    inherit (pkgs.darwin) autoSignDarwinBinariesHook;
  };

  # Tmux with a custom tmux.conf baked in
  tmux = callPackage ./tmux { };
  neovimMasterUnwrapped = pkgs.neovim.unwrapped.overrideAttrs (_: {
    version = "master-${pkgs.neovim-src.shortRev or (builtins.substring 0 7 pkgs.neovim-src.rev)}";
    src = pkgs.neovim-src;
    # versionCheckHook expects a release-style version string, but master
    # reports upstream's dev version (for example v0.13.0-dev).
    doCheck = false;
    doInstallCheck = false;
    meta = pkgs.neovim.unwrapped.meta // {
      changelog = "https://github.com/neovim/neovim/commits/${pkgs.neovim-src.rev}";
    };
  });
  neovimWithPython = pkgs.wrapNeovim neovimMasterUnwrapped {
    withPython3 = true;
    extraPython3Packages =
      ps: with ps; [
        pynvim
      ];
  };
  pythonWithPynvim = pkgs.python3.withPackages (
    ps: with ps; [
      pynvim
    ]
  );
  zshForProfile =
    if stdenv.isDarwin then
      let
        # zsh 5.9 regenerated with Autoconf 2.73 in nixpkgs_3 c4073437 hangs
        # in interactive external command substitutions and compdump on Darwin.
        oldPkgs = import (builtins.getFlake "github:NixOS/nixpkgs/f731538cdf1410a3c53d3a75a6a1142afc08e3af") {
          inherit (pkgs.stdenv.hostPlatform) system;
        };
        oldAutoreconfHook = pkgs.autoreconfHook.override {
          autoconf = oldPkgs.autoconf;
        };
      in
      pkgs.zsh.overrideAttrs (old: {
        nativeBuildInputs = builtins.map (
          input:
          if (input.pname or "") == "autoreconf-hook" then
            oldAutoreconfHook
          else
            input
        ) (old.nativeBuildInputs or [ ]);
      })
    else
      pkgs.zsh;

in

with pkgs;
[
  bat
  cacert
  curl
  ocaml-ng.ocamlPackages_5_5.carl
  eza
  ffmpeg
  git
  gnupg
  htop
  jq
  nixd
  nixfmt
  neovimWithPython
  nix-zsh-completions
  procps
  pythonWithPynvim
  silver-searcher
  stylua
  fff
  fff-cli

  # for nvim
  ripgrep
  fd
  tree-sitter
  zshForProfile
  terminal-notifier

  # Remote development
  devpod
  ty
  typescript-language-server
  nodejs_latest
  github-mcp-server
]
++ [
  kitty
  tmux
]
