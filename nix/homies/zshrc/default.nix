{
  lib,
  oh-my-zsh,
  writeTextDir,
  zsh-autosuggestions,
  zsh-completions,
  zsh-syntax-highlighting,
}:

let
  zshrcText = writeTextDir "share/zsh/zshrc" (
    lib.concatStringsSep "\n" [
      ''
        # Path to your oh-my-zsh installation.
        export ZSH=${oh-my-zsh}/share/oh-my-zsh
        fpath=(${zsh-completions}/share/zsh/site-functions $fpath)
      ''
      (builtins.readFile ./zshrc)
      ''
        source ${zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        source ${zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh
      ''
    ]
  );
in
zshrcText
