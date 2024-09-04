{ kitty, symlinkJoin, makeWrapper, stdenv }:

let
  conf = ./kitty.conf;
in

symlinkJoin {
  name = "kitty";
  buildInputs = [ makeWrapper ];
  paths = [ kitty ];
  postBuild = ''
    ${if stdenv.isDarwin then ''
        wrapProgram "$out/Applications/kitty.app/Contents/MacOS/kitty" --add-flags "--config ${conf}"
      '' else ""}
    wrapProgram "$out/bin/kitty" --add-flags "--config ${conf}"
  '';
}
