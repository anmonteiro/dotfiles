# Needs to run before the line after for `zsh` to be in that PATH. For some
# reason, Nix put its initialization in `~/.bash_profile`

if [ -e /Users/anmonteiro/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/anmonteiro/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

source $(nix-env -q zshrc --out-path --no-name)/bin/zshrc

export CFLAGS="-I/Users/anmonteiro/.nix-profile/include -L/Users/anmonteiro/.nix-profile/lib"
export LDFLAGS="-L/Users/anmonteiro/.nix-profile/lib"
export C_INCLUDE_PATH=~/.nix-profile/include
export LIBRARY_PATH=~/.nix-profile/lib

