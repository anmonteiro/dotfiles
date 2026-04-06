
- For any file search or grep in git indexed directories, always use fff tools

- Projects will usually have a flake.nix, so you can either:
    1. use their shell to run certain commands
    2. add stuff to their nix shell to get new commands. you can do this without approval


- if there's no flake.nix, feel free to add one for each project we work on. depend on github:nix-ocaml/nix-overlays and use flake systems exposed genattrs, don't bring in flake-utils stuff.
