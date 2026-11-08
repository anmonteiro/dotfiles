# Needs to run before the line after for `zsh` to be in that PATH. For some
# reason, Nix put its initialization in `~/.bash_profile`

if [ -e /Users/anmonteiro/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/anmonteiro/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

source "$HOME/.nix-profile/share/zsh/zshrc"

_git_gone_local_branches() {
  git branch --format='%(refname:short) %(upstream:track)' |
    awk '$2 == "[gone]" {print $1}'
}

git-prune-gone-branches() {
  git fetch --all --prune || return

  local branch current deleted=0 skipped=0
  current="$(git branch --show-current)"

  while IFS= read -r branch; do
    if [[ "$branch" == "$current" ]]; then
      printf 'Skipping %s (current branch)\n' "$branch"
      (( skipped++ ))
    elif git merge-base --is-ancestor "$branch" HEAD; then
      git branch -d "$branch" && (( deleted++ ))
    else
      printf 'Skipping %s (not merged into HEAD; use gprune-gone-force to delete)\n' "$branch"
      (( skipped++ ))
    fi
  done < <(_git_gone_local_branches)

  (( deleted || skipped )) || printf 'No gone local branches.\n'
}

git-prune-gone-branches-force() {
  git fetch --all --prune || return

  local branch current deleted=0 skipped=0
  current="$(git branch --show-current)"

  while IFS= read -r branch; do
    if [[ "$branch" == "$current" ]]; then
      printf 'Skipping %s (current branch)\n' "$branch"
      (( skipped++ ))
    else
      git branch -D "$branch" && (( deleted++ ))
    fi
  done < <(_git_gone_local_branches)

  (( deleted || skipped )) || printf 'No gone local branches.\n'
}

alias gprune-gone='git-prune-gone-branches'
alias gprune-gone-force='git-prune-gone-branches-force'

