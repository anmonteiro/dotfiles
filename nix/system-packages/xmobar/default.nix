{xmobar, symlinkJoin, makeWrapper}:
symlinkJoin {
  name = "xmobar";
  buildInputs = [makeWrapper];
  paths = [ xmobar ];
  postBuild = ''
    wrapProgram "$out/bin/xmobar" --add-flags "${./xmobarrc}"
  '';
}

