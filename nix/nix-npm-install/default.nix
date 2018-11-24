{writeScriptBin, pkgs}:
let
  # bashrc = writeText ""
      # (lib.concatStringsSep "\n"
      # [ (builtins.readFile ./bashrc)
        # ''
        # source ${fzf}/share/fzf/completion.bash
        # source ${fzf}/share/fzf/key-bindings.bash
        # ''
      # ]
      # )
  # nix-npm-install = writeScriptBin "nix-npm-install" (builtins.readFile ./foo.sh);

    # tempdir="/tmp/nix-npm-install/$1"
    # mkdir -p $tempdir
    # pushd $tempdir
    # ${pkgs.nodePackages.node2nix}/bin/node2nix --nodejs-10 --input <( echo "[\"$1\"]")
    # nix-env --install --file .
    # popd

  nix-npm-install = writeScriptBin "nix-npm-install" ''
    #!/usr/bin/env bash
    PACKAGE=(''${1//@/ })
    LEN=''${#PACKAGE[*]}
    tempdir="/tmp/nix-npm-install/''${PACKAGE[0]}"

    if [[ $LEN > 1 ]]; then
      INPUT="[{\"''${PACKAGE[0]}\": \"''${PACKAGE[1]}\"}]"
    else
      INPUT="[\"$PACKAGE\"]"
    fi

    mkdir -p $tempdir
    pushd $tempdir
    ${pkgs.nodePackages.node2nix}/bin/node2nix --nodejs-10 --input <( echo $INPUT)
    nix-env --install --file .
    popd


    '';

in nix-npm-install
