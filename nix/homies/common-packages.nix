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
  # Neovim's treesitter functional tests on Darwin can collide on short `--listen`
  # names when XDG_RUNTIME_DIR falls back to a long temp path.
  patchedNeovimUnwrapped = pkgs.neovim.unwrapped.overrideAttrs (old: {
    preCheck = lib.concatStringsSep "\n" [
      (old.preCheck or "")
      ''
        export XDG_RUNTIME_DIR="$NIX_BUILD_TOP/xdg-runtime"
        mkdir -p "$XDG_RUNTIME_DIR"
        chmod 700 "$XDG_RUNTIME_DIR"
      ''
    ];
  });
  neovimWithPython = pkgs.wrapNeovim patchedNeovimUnwrapped {
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
  zsh
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
