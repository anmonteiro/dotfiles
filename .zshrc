# Needs to run before the line after for `zsh` to be in that PATH. For some
# reason, Nix put its initialization in `~/.bash_profile`
source ~/.bash_profile
source $(nix-env -q zshrc --out-path --no-name)/bin/zshrc
