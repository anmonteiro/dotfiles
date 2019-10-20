{writeText, writeScriptBin, lib, pkgs}:
let
  zshrc = writeScriptBin "zshrc"
    (lib.concatStringsSep "\n"
    [ ''
      # Path to your oh-my-zsh installation.
      export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh
      fpath=(${pkgs.zsh-completions}/share/zsh/site-functions $fpath)
      ''
      (builtins.readFile ./zshrc)
      ''
      source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      source ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh
      #source ${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
      ''
    ]
    );
in zshrc
