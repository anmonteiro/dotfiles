{ kitty, symlinkJoin, makeWrapper, stdenv }:

let
  conf = ./kitty.conf;
  kittyPatched = kitty.overrideAttrs (_: {
    postPatch = ''
      mkdir fonts
      cp ${./SymbolsNerdFontMono-Regular.ttf} fonts/SymbolsNerdFontMono-Regular.ttf
    '';
  });

in

symlinkJoin {
  name = "kitty";
  buildInputs = [ makeWrapper ];
  paths = [ kittyPatched ];
  postBuild = ''
    ${if stdenv.isDarwin then ''
        wrapProgram "$out/Applications/kitty.app/Contents/MacOS/kitty" --add-flags "--config ${conf}"
      '' else ""}
    wrapProgram "$out/bin/kitty" --add-flags "--config ${conf}"
  '';
}
