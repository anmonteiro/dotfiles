{ neovim
, git
, cacert
, symlinkJoin
, makeWrapper
, stdenv
, copyPathToStore
, fetchurl
, linux-user ? null
}:

let
  homeDir =
    if stdenv.isLinux then
      linux-user
    else
      "/Users/anmonteiro";
  customizations = copyPathToStore ./customizations;

  ocaml-plugin = stdenv.mkDerivation {
    name = "ocaml-plugin";
    src = ./ocaml-plugin;
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out
      cp -r $src/* $out/
    '';
  };

  vimPlug = stdenv.mkDerivation {
    name = "vim-plug";
    src = fetchurl {
      url = https://raw.githubusercontent.com/junegunn/vim-plug/d80f495fabff8446972b8695ba251ca636a047b0/plug.vim;
      sha256 = "1nywzjd9nfr7sqqbdi69wza305q3vp26i0390j1884wdz6awid10";
    };
    unpackPhase = ''
      mkdir -p $out/autoload
      cp $src $out/autoload/plug.vim
    '';
    dontConfigure = true;
    dontBuild = true;
    dontInstall = true;
  };

  lua-modules = stdenv.mkDerivation {
    name = "lua-modules";
    src = ./lua;
    dontConfigure = true;
    dontBuild = true;

    # Lua modules are found inside a lua/ folder in your 'runtimepath'
    installPhase = ''
      mkdir -p $out/lua
      cp -r $src/* $out/lua/
    '';
  };
in
symlinkJoin {
  name = "nvim";
  buildInputs = [ makeWrapper ];
  paths = [ neovim ];
  postBuild = ''
    wrapProgram "$out/bin/nvim" \
      --add-flags "--cmd 'set rtp+=${vimPlug},${ocaml-plugin},${lua-modules}' -u ${./init.vim}" \
      --set NVIM_CONFIG_CUSTOMIZATIONS_PATH "${customizations}" \
      --set NVIM_CONFIG_PLUGINS_PATH "${homeDir}/.config/nvim/plugged" \
  '';
}
