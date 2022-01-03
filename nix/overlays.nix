let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  src = builtins.fetchGit {
    url = with lock.nodes.anmonteiro.locked; "https://github.com/${owner}/${repo}";
    inherit (lock.nodes.anmonteiro.locked) rev;
    # inherit (lock.nodes.anmonteiro.original) ref;
  };

in

src
