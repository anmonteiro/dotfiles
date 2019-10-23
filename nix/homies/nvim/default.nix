{ neovim
, git
, cacert
, symlinkJoin
, makeWrapper
, stdenv
, copyPathToStore
, fzf
, fetchurl
, config }:

let
  customizations = copyPathToStore ./customizations;

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
    buildInputs = [ makeWrapper cacert git ];
    paths = [ neovim ];
    NVIM_RPLUGIN_MANIFEST = "NONE";
    postBuild = ''
      wrapProgram "$out/bin/nvim" \
        --add-flags "--cmd 'set rtp+=${vimPlug}' -u ${./init.vim}" \
        --set NVIM_CONFIG_CUSTOMIZATIONS_PATH "${customizations}" \
        --set NVIM_CONFIG_PLUGINS_PATH "$out/.config/nvim/plugged" \
        --set NVIM_CONFIG_FZF_PATH "${fzf}"
      $out/bin/nvim --headless -i NONE -n +PlugInstall +qall
    '';
  }
