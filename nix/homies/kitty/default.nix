{ kitty
, symlinkJoin
, stdenv
, makeBinaryWrapper
, autoSignDarwinBinariesHook
}:

let
  conf = ./kitty.conf;
in

symlinkJoin {
  name = "kitty";
  nativeBuildInputs = [ makeBinaryWrapper autoSignDarwinBinariesHook ];
  paths = [ kitty ];
  postBuild = ''
    ${if stdenv.isDarwin then ''
        wrapProgram "$out/Applications/kitty.app/Contents/MacOS/kitty" --add-flags "--config ${conf}"
      '' else ""}
    wrapProgram "$out/bin/kitty" --add-flags "--config ${conf}"
  '';
}
