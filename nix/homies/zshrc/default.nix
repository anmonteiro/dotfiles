{ fzf
, lib
, oh-my-zsh
, sqlite
, stdenv
, writeText
, writeScriptBin
, zsh-autosuggestions
, zsh-completions
, zsh-syntax-highlighting
}:

let
  zshrc = writeScriptBin "zshrc"
    (lib.concatStringsSep "\n"
      [
        ''
          # Path to your oh-my-zsh installation.
          export ZSH=${oh-my-zsh}/share/oh-my-zsh
          fpath=(${zsh-completions}/share/zsh/site-functions $fpath)
        ''
        (builtins.readFile ./zshrc)
        ''
          source ${zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
          source ${zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh
          source ${fzf}/share/fzf/key-bindings.zsh

          # Only show 10 lines of results in FZF reverse-i-search
          # FZF_DEFAULT_OPTS='--hidden'
          for i in FZF_CTRL_R_OPTS FZF_ALT_C_OPTS FZF_CTRL_T_OPTS; do
            export $i='--height 12'
          done

          # https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh
          for i in FZF_ALT_C_COMMAND FZF_CTRL_T_COMMAND; do
            export $i="command find -L . -mindepth 1 \
              \\( -path '*/\\.svn' \
                  -o -path '*/\\.git' \
                  -o -path '*/\\.hg' \
                  -o -fstype 'sysfs' \
                  -o -fstype 'devfs' \
                  -o -fstype 'devtmpfs' \
                  -o -fstype 'proc' \\) \
              -prune \
              -o -type f -print \
              -o -type d -print \
              -o -type l -print 2> /dev/null | cut -b3-"
          done

          # For telescope smart-open
          export LIBSQLITE=${
            if stdenv.isDarwin
            then "/usr/lib/sqlite3/libtclsqlite3.dylib"
            else "${sqlite.out}/lib/libsqlite3.so"
          }
        ''
      ]
    );
in
zshrc
