{kitty, symlinkJoin, makeWrapper}:
symlinkJoin {
  name = "kitty";
  buildInputs = [makeWrapper];
  paths = [ kitty ];
  postBuild = ''
    wrapProgram "$out/bin/kitty" --add-flags "--config ${./kitty.conf}"
  '';
}

