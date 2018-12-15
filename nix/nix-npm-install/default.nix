{writeScriptBin, pkgs}:
let
  nix-npm-install = writeScriptBin "nix-npm-install" ''
    #!/usr/bin/env bash
    PACKAGE=($(echo ''${1} | awk '{split(''$0,a,"@"); print a[1],a[2],a[3]}'))
    LEN=''${#PACKAGE[*]}
    tempdir="/tmp/nix-npm-install/''${PACKAGE[0]}"

    function pkg_name () {
      PKG_NAME=''$(sed -e 's/^[[:space:]]*//' <(echo $1))
      if [[ ''$2 == @* ]]; then
        PKG_NAME="@''${PKG_NAME}"
      else
        PKG_NAME=''${PKG_NAME}
      fi

      echo ''$PKG_NAME
    }

    if [[ $LEN > 1 ]]; then
      PKG_NAME=$(pkg_name ''${PACKAGE[0]} $1)
      INPUT="[{\"''${PKG_NAME}\": \"''${PACKAGE[1]}\"}]"
    else
      PKG_NAME=$(pkg_name $PACKAGE $1)
      INPUT="[\"$PKG_NAME\"]"
    fi

    echo $INPUT $tempdir

    mkdir -p $tempdir
    pushd $tempdir
    ${pkgs.nodePackages.node2nix}/bin/node2nix --nodejs-10 --input <( echo $INPUT)
    nix-env --install --file .
    popd
    '';

in nix-npm-install
