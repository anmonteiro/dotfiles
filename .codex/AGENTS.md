
- For any file search or grep in git indexed directories, always use the fff MCP (not the fff CLI, that's a different thing for my local use!)

- Projects will usually have a flake.nix, so you can either:
    1. use their shell to run certain commands
    2. add stuff to their nix shell to get new commands. you can do this without approval


- if there's no flake.nix, add one for each project we work on. depend on github:nix-ocaml/nix-overlays and use flake systems exposed genattrs, don't bring in flake-utils stuff.

## Working with Git

- When opening Pull Requests, don't add verbose descriptions. State only the purpose of the change, and whether it relates to other work (referencing other PRs, issues, commits, etc)
- When opening Pull requests, always include the Codex session UUID in the PR description, but only the UUID without further context
