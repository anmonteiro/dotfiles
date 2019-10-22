{ neovim
, git
, cacert
, symlinkJoin
, makeWrapper
, stdenv
, copyPathToStore
, fetchurl
, config }:

let
  homeDir =
    if stdenv.system == "x86_64-darwin" then
      "/Users/anmonteiro"
    else
      config.users.users.anmonteiro.home;

  cachePath = "${homeDir}/.cache/dein";

  deinInstaller = stdenv.mkDerivation {
    name = "dein";
    src = fetchurl {
      url = https://raw.githubusercontent.com/Shougo/dein.vim/916f755/bin/installer.sh;
      sha256 = "605e0f86a1c7f6f63112c38973de4a3411eddec2c899d009d648548bb7a7a962";
      executable = true;
    };
    buildInputs = [ git cacert ];
    unpackPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/installer.sh
    '';
    dontConfigure = true;
    buildPhase = ''
      mkdir $out/cache
      env
      pwd
      ls $out/bin
      $out/bin/installer.sh $out/cache
    '';
    installPhase = ''
      rm -rf ${cachePath}
      mkdir -p ${cachePath}

      cp -R $out/cache/* ${cachePath}
    '';
  };

  customizations = copyPathToStore ./customizations;

in
  symlinkJoin {
    name = "nvim";
    buildInputs = [ makeWrapper deinInstaller ];
    paths = [ neovim ];
    postBuild = ''
      wrapProgram "$out/bin/nvim" \
        --add-flags "-u ${./init.vim}" \
        --set NVIM_CONFIG_CUSTOMIZATIONS_PATH "${customizations}" \
        --set NVIM_CONFIG_DEIN_CACHE "${cachePath}"
    '';
  }
