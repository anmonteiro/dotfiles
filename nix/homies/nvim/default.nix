{ neovim
, git
, cacert
, symlinkJoin
, makeWrapper
, stdenv
, copyPathToStore
, fzf
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
      url = https://raw.githubusercontent.com/junegunn/vim-plug/034e8445908e828351da6e428022d8487c57ce99/plug.vim;
      sha256 = "1vcx8cn8y9v5zrl63par0w22pv0kk3c7avpwc7ca77qsr2p0nz5r";
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
      --set NVIM_CONFIG_FZF_PATH "${fzf}"
  '';
}
