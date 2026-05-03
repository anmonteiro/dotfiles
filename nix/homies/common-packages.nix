{
  pkgs,
  callPackage,
  lib,
  stdenv,
}:

let
  # Kitty with a custom kitty.conf baked in
  kitty = callPackage ./kitty {
    inherit (pkgs.darwin) autoSignDarwinBinariesHook;
  };

  # Tmux with a custom tmux.conf baked in
  tmux = callPackage ./tmux { };
  # Neovim's tests belong to the unwrapped derivation. Overriding the wrapper
  # leaves pkgs.neovim.passthru.unwrapped.doCheck enabled.
  neovimUnwrappedNoChecks = pkgs.neovim-unwrapped.overrideAttrs (old: {
    doCheck = false;
    doInstallCheck = false;
    preCheck = lib.concatStringsSep "\n" [
      (old.preCheck or "")
      ''
        export XDG_RUNTIME_DIR="$NIX_BUILD_TOP/xdg-runtime"
        mkdir -p "$XDG_RUNTIME_DIR"
        chmod 700 "$XDG_RUNTIME_DIR"
      ''
    ];
  });
  neovimWithPython = pkgs.wrapNeovim neovimUnwrappedNoChecks {
    withPython3 = true;
    extraPython3Packages =
      ps: with ps; [
        pynvim
      ];
  };
  zshForProfile =
    if stdenv.isDarwin then
      # zsh 5.9 regenerated with Autoconf 2.73 tries C23 on Darwin, which hangs
      # in interactive external command substitutions and compdump.
      pkgs.zsh.overrideAttrs (old: {
        configureFlags = (old.configureFlags or [ ]) ++ [ "ac_cv_prog_cc_c23=no" ];
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
  bun
]
++ [
  kitty
  tmux
]
