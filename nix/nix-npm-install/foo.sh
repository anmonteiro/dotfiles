#!/usr/bin/env bash
PACKAGE=(${1//@/ })
LEN=${#PACKAGE[*]}
tempdir="/tmp/nix-npm-install/${PACKAGE[0]}"

if [[ $LEN > 1 ]]; then
  INPUT="[{\"${PACKAGE[0]}\": \"${PACKAGE[1]}\"}]"
else
  INPUT="[\"$PACKAGE\"]"
fi

mkdir -p $tempdir
pushd $tempdir
${pkgs.nodePackages.node2nix}/bin/node2nix --nodejs-10 --input <( echo $INPUT)
nix-env --install --file .
popd

