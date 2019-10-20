# NOTE: this assumes `dein.vim` has been installed to ~/.cache/dein.
# It's future work to move to a more "static" package manager.
{neovim, symlinkJoin, makeWrapper, copyPathToStore}:

let customizations = copyPathToStore ./customizations;
in
symlinkJoin {
  name = "nvim";
  buildInputs = [makeWrapper];
  paths = [ neovim ];
  postBuild = ''
    wrapProgram "$out/bin/nvim" --add-flags "-u ${./init.vim}" --set HACK_CUSTOMIZATIONS_PATH "${customizations}"
  '';
}

