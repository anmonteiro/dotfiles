{ neovim
, git
, cacert
, symlinkJoin
, makeWrapper
, stdenv
, copyPathToStore
, fzf
, fetchurl
, config
}:

let
  homeDir = if stdenv.isLinux then
    config.users.users.anmonteiro.home
  else
    "/Users/anmonteiro";
  customizations = copyPathToStore ./customizations;

  ftPlugin = stdenv.mkDerivation {
    name = "ftPlugin";
    src = ./ftplugin;
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      ls $src
      mkdir -p $out/ftplugin
      ls $out
      cp $src/* $out/ftplugin
    '';
  };

  vimPlug = stdenv.mkDerivation {
    name = "vim-plug";
    src = fetchurl {
      url = https://raw.githubusercontent.com/junegunn/vim-plug/eee50c5/plug.vim;
      sha256 = "161lkcdjgy2lbg2ld89p4h1pawd4m8s8rllvsp68rq0457ahynpl";
    };
    unpackPhase = ''
      mkdir -p $out/autoload
      cp $src $out/autoload/plug.vim
    '';
    dontConfigure = true;
    dontBuild = true;
    dontInstall = true;
  };

  # myNeovim = neovim.override {
    # vimAlias = true;
    # withPython = true;
  # };

in
  symlinkJoin {
    name = "nvim";
    buildInputs = [ makeWrapper ];
    paths = [ neovim ];
    postBuild = ''
      wrapProgram "$out/bin/nvim" \
        --add-flags "--cmd 'set rtp+=${vimPlug},${ftPlugin}' -u ${./init.vim}" \
        --set NVIM_CONFIG_CUSTOMIZATIONS_PATH "${customizations}" \
        --set NVIM_CONFIG_PLUGINS_PATH "${homeDir}/.config/nvim/plugged" \
        --set NVIM_CONFIG_FZF_PATH "${fzf}"
    '';
  }
